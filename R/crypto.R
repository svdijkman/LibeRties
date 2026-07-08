# ---------------------------------------------------------------------------
# Application-level, per-tenant envelope encryption for GDPR-sensitive job data.
#
# Design (see SECURITY.md):
#   * User key (UK) is derived from the user's plaintext API token + a per-user
#     random salt (scrypt KDF). The salt is non-secret and lives in users.json.
#     The token itself is never stored (only its SHA-256 hash, for auth).
#   * Each job gets a random 32-byte data-encryption key (DEK). The DEK encrypts
#     the job payload (args) and result via libsodium secretbox (XSalsa20-
#     Poly1305 AEAD). The DEK is wrapped (encrypted) with the UK and stored as
#     key.enc next to the ciphertext.
#   * The API derives the UK on-demand from the authenticated request token and
#     caches it in process memory for the user; it is never written to disk.
#     Losing the token => the data is unrecoverable, by design.
# ---------------------------------------------------------------------------

#' @keywords internal
.ls_crypto_available <- function() {
  requireNamespace("sodium", quietly = TRUE)
}

#' @keywords internal
.ls_crypto_require <- function() {
  if (!.ls_crypto_available()) {
    stop(
      "Package 'sodium' is required for at-rest encryption of GDPR-sensitive ",
      "data. Install it with install.packages('sodium').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Whether at-rest encryption is enabled (config + sodium present).
#' @keywords internal
.ls_encryption_enabled <- function(cfg = NULL) {
  if (!.ls_crypto_available()) {
    return(FALSE)
  }
  if (is.null(cfg)) {
    cfg <- ls_config()
  }
  isTRUE(cfg$encrypt_at_rest %||% TRUE)
}

#' @keywords internal
.ls_bin2hex <- function(raw) {
  .ls_crypto_require()
  sodium::bin2hex(raw)
}

#' @keywords internal
.ls_hex2bin <- function(hex) {
  .ls_crypto_require()
  sodium::hex2bin(as.character(hex))
}

#' Derive the 32-byte user key from a plaintext token and per-user salt.
#' @keywords internal
.ls_derive_uk <- function(token, salt) {
  .ls_crypto_require()
  salt_raw <- if (is.raw(salt)) salt else sodium::hex2bin(as.character(salt))
  if (length(salt_raw) != 32L) {
    stop("Encryption salt must be 32 bytes.", call. = FALSE)
  }
  sodium::scrypt(charToRaw(as.character(token)), salt = salt_raw, size = 32L)
}

#' Generate a fresh random 32-byte salt (hex) for a user.
#' @keywords internal
.ls_new_salt_hex <- function() {
  .ls_crypto_require()
  sodium::bin2hex(sodium::random(32L))
}

#' Generate a fresh random data-encryption key (32 bytes).
#' @keywords internal
.ls_dek_generate <- function() {
  .ls_crypto_require()
  sodium::keygen()
}

#' AEAD-encrypt an arbitrary R object to a self-describing envelope list.
#' @keywords internal
.ls_aead_encrypt_obj <- function(obj, key) {
  .ls_crypto_require()
  msg <- serialize(obj, connection = NULL)
  nonce <- sodium::random(24L)
  ct <- sodium::data_encrypt(msg, key, nonce)
  attr(ct, "nonce") <- NULL
  list(v = 1L, alg = "secretbox", nonce = nonce, ct = as.raw(ct))
}

#' @keywords internal
.ls_aead_decrypt_obj <- function(envelope, key) {
  .ls_crypto_require()
  pt <- sodium::data_decrypt(envelope$ct, key, envelope$nonce)
  unserialize(pt)
}

#' Encrypt an object with a key and persist the envelope to a file.
#' @keywords internal
.ls_encrypt_to_file <- function(obj, key, path) {
  .ls_save_rds_safe(.ls_aead_encrypt_obj(obj, key), path)
}

#' Read and decrypt an envelope file with a key.
#' @keywords internal
.ls_decrypt_from_file <- function(key, path) {
  env <- .ls_read_rds_safe(path)
  if (is.null(env)) {
    stop("Encrypted file missing or unreadable: ", path, call. = FALSE)
  }
  .ls_aead_decrypt_obj(env, key)
}

#' Wrap (encrypt) a DEK with a user key.
#' @keywords internal
.ls_dek_wrap <- function(dek, uk) {
  .ls_aead_encrypt_obj(dek, uk)
}

#' Unwrap (decrypt) a DEK with a user key.
#' @keywords internal
.ls_dek_unwrap <- function(envelope, uk) {
  .ls_aead_decrypt_obj(envelope, uk)
}

# ---------------------------------------------------------------------------
# In-memory user-key cache. Populated whenever a request authenticates (so the
# key exists only while the owner is actively using the server) and never
# persisted. Keyed by username -> raw 32-byte UK.
# ---------------------------------------------------------------------------

#' @keywords internal
.ls_uk_cache <- new.env(parent = emptyenv())

#' Remember a user's derived key for this server process (from an authed token).
#' @keywords internal
.ls_uk_remember <- function(username, token) {
  if (!.ls_crypto_available()) {
    return(invisible(NULL))
  }
  salt <- tryCatch(.ls_user_enc_salt(username), error = function(e) NULL)
  if (is.null(salt)) {
    return(invisible(NULL))
  }
  uk <- tryCatch(.ls_derive_uk(token, salt), error = function(e) NULL)
  if (!is.null(uk)) {
    assign(username, uk, envir = .ls_uk_cache)
  }
  invisible(NULL)
}

#' Retrieve a cached user key, or NULL if the owner has not authenticated.
#' @keywords internal
.ls_uk_get <- function(username) {
  if (exists(username, envir = .ls_uk_cache, inherits = FALSE)) {
    get(username, envir = .ls_uk_cache)
  } else {
    NULL
  }
}

#' Forget a cached user key (e.g. on token rotation).
#' @keywords internal
.ls_uk_forget <- function(username) {
  if (exists(username, envir = .ls_uk_cache, inherits = FALSE)) {
    rm(list = username, envir = .ls_uk_cache)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Filesystem hardening. Best-effort owner-only ACLs on the sandbox tree to blunt
# a non-privileged co-tenant OS user reading another tenant's data on disk. This
# complements (does not replace) at-rest encryption. Never fails the caller.
# ---------------------------------------------------------------------------

#' Encrypt a user's existing plaintext job payloads/results in place.
#'
#' Migration helper for deployments that already ran jobs before at-rest
#' encryption was enabled. Requires the user's current token to derive their
#' key. Converts args.rds -> args.enc and result.rds -> result.enc (wrapping a
#' fresh per-job DEK) and removes the plaintext files. Clean-start users can
#' ignore this - new jobs are encrypted automatically.
#'
#' @param username User whose jobs to migrate.
#' @param token The user's current API token (needed to derive their key).
#' @return Number of jobs migrated (invisibly).
#' @export
ls_user_encrypt_existing <- function(username, token) {
  .ls_crypto_require()
  username <- .ls_sanitize_user(username)
  users <- .ls_users_load()
  u <- users[[username]]
  if (is.null(u)) {
    stop("Unknown user: ", username, call. = FALSE)
  }
  if (!identical(.ls_hash_token(as.character(token)), u$token_hash)) {
    stop("Token does not match user '", username, "'.", call. = FALSE)
  }
  salt <- .ls_user_enc_salt(username)
  uk <- .ls_derive_uk(token, salt)
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(invisible(0L))
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  migrated <- 0L
  for (id in ids) {
    jp <- .ls_job_path(username, id)
    args_rds <- file.path(jp, "args.rds")
    res_rds <- file.path(jp, "result.rds")
    if (!file.exists(args_rds) && !file.exists(res_rds)) {
      next
    }
    dek <- if (file.exists(file.path(jp, "key.enc"))) {
      tryCatch(.ls_dek_unwrap(.ls_read_rds_safe(file.path(jp, "key.enc")), uk),
               error = function(e) NULL)
    } else {
      NULL
    }
    if (is.null(dek)) {
      dek <- .ls_dek_generate()
      .ls_save_rds_safe(.ls_dek_wrap(dek, uk), file.path(jp, "key.enc"))
    }
    if (file.exists(args_rds)) {
      obj <- .ls_read_rds_safe(args_rds)
      .ls_encrypt_to_file(obj, dek, file.path(jp, "args.enc"))
      unlink(args_rds)
    }
    if (file.exists(res_rds)) {
      obj <- .ls_read_rds_safe(res_rds)
      .ls_encrypt_to_file(obj, dek, file.path(jp, "result.enc"))
      unlink(res_rds)
    }
    # Mark meta as encrypted for consistency.
    meta_path <- file.path(jp, "meta.rds")
    meta <- .ls_read_rds_safe(meta_path)
    if (!is.null(meta)) {
      meta$encrypted <- TRUE
      .ls_save_rds_safe(meta, meta_path)
    }
    migrated <- migrated + 1L
  }
  message("Encrypted ", migrated, " existing job(s) for '", username, "'.")
  invisible(migrated)
}

#' Restrict a directory to the owning OS account (best-effort, non-fatal).
#' @keywords internal
.ls_secure_dir <- function(path) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    return(invisible(FALSE))
  }
  if (isFALSE(getOption("LibeRties.harden_acls", TRUE))) {
    return(invisible(FALSE))
  }
  ok <- tryCatch({
    if (.Platform$OS.type == "windows") {
      user <- Sys.getenv("USERNAME", "")
      if (nzchar(user)) {
        # Disable inheritance (copy), grant only the owner + SYSTEM full control,
        # and remove the broad Users group. Quiet; ignore individual failures.
        p <- normalizePath(path, winslash = "\\", mustWork = FALSE)
        suppressWarnings(system2("icacls", c(shQuote(p), "/inheritance:r"),
                                 stdout = FALSE, stderr = FALSE))
        suppressWarnings(system2("icacls", c(shQuote(p), "/grant:r",
                                 paste0(user, ":(OI)(CI)F"), "SYSTEM:(OI)(CI)F"),
                                 stdout = FALSE, stderr = FALSE))
        suppressWarnings(system2("icacls", c(shQuote(p), "/remove:g", "Users", "Everyone"),
                                 stdout = FALSE, stderr = FALSE))
      }
    } else {
      Sys.chmod(path, mode = "0700", use_umask = FALSE)
    }
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}
