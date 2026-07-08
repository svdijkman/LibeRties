#' @keywords internal
.ls_users_path <- function() {
  .ls_path("users.json")
}

#' @keywords internal
.ls_admin_token_path <- function() {
  .ls_path("admin", "token_hash.txt")
}

#' List scheduler users and limits
#'
#' @return Data frame of users (token hashes omitted).
#' @export
ls_user_list <- function() {
  .ls_init_storage()
  users <- .ls_read_json(.ls_users_path(), list())
  if (length(users) == 0L) {
    return(data.frame(
      username = character(),
      enabled = logical(),
      running_jobs = integer(),
      queued_jobs = integer(),
      max_concurrent_jobs = integer(),
      max_disk_mb = integer(),
      max_cpu = integer(),
      max_memory_mb = integer(),
      created = character(),
      stringsAsFactors = FALSE
    ))
  }
  nms <- names(users)
  data.frame(
    username = nms,
    enabled = vapply(users, function(u) isTRUE(u$enabled), logical(1L)),
    running_jobs = vapply(nms, function(u) .ls_user_running_jobs(u), integer(1L)),
    queued_jobs = vapply(nms, function(u) .ls_user_queued_jobs(u), integer(1L)),
    max_concurrent_jobs = vapply(users, function(u) as.integer(u$max_concurrent_jobs %||% 2L), integer(1L)),
    max_disk_mb = vapply(users, function(u) as.integer(u$max_disk_mb %||% 5120L), integer(1L)),
    max_cpu = vapply(users, function(u) as.integer(u$max_cpu %||% 4L), integer(1L)),
    max_memory_mb = vapply(users, function(u) as.integer(u$max_memory_mb %||% 8192L), integer(1L)),
    created = vapply(users, function(u) as.character(u$created %||% ""), character(1L)),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.ls_users_load <- function() {
  .ls_read_json(.ls_users_path(), list())
}

#' @keywords internal
.ls_users_save <- function(users) {
  .ls_write_json(users, .ls_users_path())
}

#' Get (creating if needed) a user's non-secret encryption salt (hex).
#'
#' The salt is required to derive the user key from their token. Existing users
#' created before encryption was enabled get a salt lazily on first use.
#' @keywords internal
.ls_user_enc_salt <- function(username) {
  username <- .ls_sanitize_user(username)
  users <- .ls_users_load()
  u <- users[[username]]
  if (is.null(u)) {
    stop("Unknown user: ", username, call. = FALSE)
  }
  salt <- as.character(u$enc_salt %||% "")
  if (!nzchar(salt)) {
    salt <- .ls_new_salt_hex()
    u$enc_salt <- salt
    users[[username]] <- u
    .ls_users_save(users)
  }
  salt
}

#' Create a scheduler user
#'
#' @param username Login name.
#' @param limits Optional list with \code{max_concurrent_jobs}, \code{max_disk_mb},
#'   \code{max_cpu}, \code{max_memory_mb}.
#' @param enabled Whether the account is active.
#' @return List with \code{username}, \code{token} (shown once), and \code{limits}.
#' @export
ls_user_create <- function(username, limits = list(), enabled = TRUE) {
  .ls_init_storage()
  username <- .ls_sanitize_user(username)
  users <- .ls_users_load()
  if (!is.null(users[[username]])) {
    stop("User already exists: ", username, call. = FALSE)
  }
  defs <- .ls_default_limits()
  lim <- modifyList(defs, limits)
  token <- .ls_generate_token()
  users[[username]] <- list(
    enabled = isTRUE(enabled),
    max_concurrent_jobs = as.integer(lim$max_concurrent_jobs),
    max_disk_mb = as.integer(lim$max_disk_mb),
    max_cpu = as.integer(lim$max_cpu),
    max_memory_mb = as.integer(lim$max_memory_mb),
    token_hash = .ls_hash_token(token),
    enc_salt = if (.ls_crypto_available()) .ls_new_salt_hex() else "",
    created = .ls_now()
  )
  .ls_users_save(users)
  dir.create(.ls_user_jobs_root(username), recursive = TRUE, showWarnings = FALSE)
  .ls_secure_dir(.ls_user_sandbox(username))
  list(username = username, token = token, limits = lim)
}

#' Remove a user and optionally their sandbox
#'
#' @param username User to remove.
#' @param remove_sandbox If TRUE, delete the user's job directory.
#' @export
ls_user_remove <- function(username, remove_sandbox = FALSE) {
  username <- .ls_sanitize_user(username)
  users <- .ls_users_load()
  if (is.null(users[[username]])) {
    stop("Unknown user: ", username, call. = FALSE)
  }
  users[[username]] <- NULL
  .ls_users_save(users)
  if (isTRUE(remove_sandbox)) {
    sb <- .ls_user_sandbox(username)
    if (dir.exists(sb)) {
      unlink(sb, recursive = TRUE)
    }
  }
  invisible(TRUE)
}

#' Update per-user resource limits
#'
#' @param username User name.
#' @param max_concurrent_jobs Maximum concurrently running jobs (additional jobs wait in queue).
#' @param max_disk_mb Sandbox disk quota in MB.
#' @param max_cpu CPU cores per container/worker.
#' @param max_memory_mb Memory limit in MB per job.
#' @param enabled Optional account enable flag.
#' @export
ls_user_set_limits <- function(username,
                               max_concurrent_jobs = NULL,
                               max_disk_mb = NULL,
                               max_cpu = NULL,
                               max_memory_mb = NULL,
                               enabled = NULL) {
  username <- .ls_sanitize_user(username)
  users <- .ls_users_load()
  u <- users[[username]]
  if (is.null(u)) {
    stop("Unknown user: ", username, call. = FALSE)
  }
  if (!is.null(max_concurrent_jobs)) u$max_concurrent_jobs <- as.integer(max_concurrent_jobs)
  if (!is.null(max_disk_mb)) u$max_disk_mb <- as.integer(max_disk_mb)
  if (!is.null(max_cpu)) u$max_cpu <- as.integer(max_cpu)
  if (!is.null(max_memory_mb)) u$max_memory_mb <- as.integer(max_memory_mb)
  if (!is.null(enabled)) u$enabled <- isTRUE(enabled)
  users[[username]] <- u
  .ls_users_save(users)
  invisible(u)
}

#' Re-wrap every encrypted job DEK for a user from one key to another.
#'
#' Used on token rotation so encrypted jobs remain readable under the new key.
#' Returns the number of key.enc files successfully re-wrapped.
#' @keywords internal
.ls_rewrap_user_deks <- function(username, uk_old, uk_new) {
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(0L)
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  n <- 0L
  for (id in ids) {
    key_path <- file.path(.ls_job_path(username, id), "key.enc")
    if (!file.exists(key_path)) {
      next
    }
    env <- .ls_read_rds_safe(key_path)
    dek <- tryCatch(.ls_dek_unwrap(env, uk_old), error = function(e) NULL)
    if (is.null(dek)) {
      next
    }
    if (isTRUE(.ls_save_rds_safe(.ls_dek_wrap(dek, uk_new), key_path))) {
      n <- n + 1L
    }
  }
  n
}

#' Issue a new API token for a user
#'
#' When at-rest encryption is enabled, the user key is derived from the token, so
#' a new token would orphan all encrypted data. Supply the user's
#' \code{current_token} to transparently re-wrap existing job keys to the new
#' token. Without it, rotation proceeds but any existing encrypted jobs become
#' permanently unrecoverable (by design).
#'
#' @param username User name.
#' @param current_token Optional current token, enabling key migration.
#' @param force If TRUE, rotate even when \code{current_token} is missing/invalid
#'   and the user has encrypted data (data becomes unrecoverable).
#' @return Character token (shown once).
#' @export
ls_user_issue_token <- function(username, current_token = NULL, force = FALSE) {
  username <- .ls_sanitize_user(username)
  users <- .ls_users_load()
  u <- users[[username]]
  if (is.null(u)) {
    stop("Unknown user: ", username, call. = FALSE)
  }

  encrypt_on <- .ls_encryption_enabled()
  has_encrypted <- FALSE
  if (encrypt_on) {
    root <- .ls_user_jobs_root(username)
    if (dir.exists(root)) {
      has_encrypted <- any(file.exists(file.path(
        list.dirs(root, full.names = TRUE, recursive = FALSE), "key.enc"
      )))
    }
  }

  new_token <- .ls_generate_token()

  if (encrypt_on && has_encrypted) {
    salt <- .ls_user_enc_salt(username)
    valid_current <- !is.null(current_token) &&
      identical(.ls_hash_token(as.character(current_token)), u$token_hash)
    if (valid_current) {
      uk_old <- .ls_derive_uk(current_token, salt)
      uk_new <- .ls_derive_uk(new_token, salt)
      n <- .ls_rewrap_user_deks(username, uk_old, uk_new)
      message("Re-wrapped ", n, " encrypted job key(s) for '", username, "'.")
    } else if (!isTRUE(force)) {
      stop(
        "User '", username, "' has encrypted jobs. Provide current_token to ",
        "migrate keys, or pass force = TRUE to rotate anyway (existing ",
        "encrypted data will become permanently unrecoverable).",
        call. = FALSE
      )
    } else {
      warning(
        "Rotating token for '", username, "' without current_token: existing ",
        "encrypted jobs are now unrecoverable.", call. = FALSE
      )
    }
  }

  u$token_hash <- .ls_hash_token(new_token)
  users[[username]] <- u
  .ls_users_save(users)
  .ls_uk_forget(username)
  new_token
}

#' Set admin API token
#'
#' @param token Plain-text admin token.
#' @export
ls_admin_token_set <- function(token) {
  .ls_init_storage()
  dir.create(dirname(.ls_admin_token_path()), recursive = TRUE, showWarnings = FALSE)
  writeLines(.ls_hash_token(token), .ls_admin_token_path())
  invisible(TRUE)
}

#' @keywords internal
.ls_admin_token_verify <- function(token) {
  path <- .ls_admin_token_path()
  if (!file.exists(path)) {
    return(FALSE)
  }
  hash <- trimws(readLines(path, warn = FALSE)[1L])
  identical(hash, .ls_hash_token(token))
}

#' @keywords internal
.ls_user_from_token <- function(token) {
  token <- trimws(as.character(token %||% ""))
  if (!nzchar(token)) {
    return(NULL)
  }
  hash <- .ls_hash_token(token)
  users <- .ls_users_load()
  for (nm in names(users)) {
    u <- users[[nm]]
    stored <- as.character(u$token_hash %||% "")
    if (identical(stored, hash)) {
      if (!isTRUE(u$enabled)) {
        stop("User account is disabled.", call. = FALSE)
      }
      return(list(username = nm, limits = u))
    }
  }
  NULL
}

#' @keywords internal
.ls_user_limits <- function(username) {
  users <- .ls_users_load()
  u <- users[[username]]
  defs <- .ls_default_limits()
  if (is.null(u)) {
    return(defs)
  }
  list(
    max_concurrent_jobs = as.integer(u$max_concurrent_jobs %||% defs$max_concurrent_jobs),
    max_disk_mb = as.integer(u$max_disk_mb %||% defs$max_disk_mb),
    max_cpu = as.integer(u$max_cpu %||% defs$max_cpu),
    max_memory_mb = as.integer(u$max_memory_mb %||% defs$max_memory_mb)
  )
}

#' @keywords internal
.ls_job_log_status_hint <- function(job_path) {
  log_path <- file.path(job_path, "worker.log")
  if (!file.exists(log_path)) {
    return("")
  }
  lines <- readLines(log_path, warn = FALSE)
  if (length(lines) == 0L) {
    return("")
  }
  tail <- lines[max(1L, length(lines) - 29L):length(lines)]
  txt <- paste(tail, collapse = "\n")
  if (grepl("Job completed\\.|Simulation completed:", txt)) {
    return("success")
  }
  if (grepl("Job failed:|Worker error:|Error in ", txt)) {
    return("error")
  }
  ""
}

#' @keywords internal
.ls_job_consumes_slot <- function(username, job_id) {
  meta <- .ls_job_read_meta(username, job_id)
  if (is.null(meta) || !identical(meta$status, "running")) {
    return(FALSE)
  }
  job_path <- .ls_job_path(username, job_id)
  if (.ls_job_has_result(job_path)) {
    return(FALSE)
  }
  if (.ls_job_worker_never_started(meta, job_path)) {
    return(FALSE)
  }
  if (.ls_job_in_start_grace(meta)) {
    return(TRUE)
  }
  .ls_job_worker_likely_running(meta, job_path)
}

#' @keywords internal
.ls_job_refresh_stale_running <- function(username) {
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(invisible(NULL))
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  for (id in ids) {
    meta <- .ls_job_read_meta(username, id)
    if (is.null(meta) || !identical(meta$status, "running")) {
      next
    }
    job_path <- .ls_job_path(username, id)
    if (.ls_job_has_result(job_path)) {
      .ls_job_status(username, id, dispatch = FALSE)
      next
    }
    log_hint <- .ls_job_log_status_hint(job_path)
    if (identical(log_hint, "success") || identical(log_hint, "error")) {
      .ls_job_status(username, id, dispatch = FALSE)
      next
    }
    if (!.ls_job_worker_likely_running(meta, job_path)) {
      .ls_job_status(username, id, dispatch = FALSE)
      next
    }
    if (.ls_job_worker_never_started(meta, job_path)) {
      meta$status <- "error"
      meta$error <- "Worker never started."
      if (is.null(meta$finished) || !nzchar(meta$finished)) {
        meta$finished <- .ls_now()
      }
      .ls_save_rds_safe(meta, file.path(job_path, "meta.rds"))
      writeLines(meta$error, file.path(job_path, "error.txt"))
      .ls_job_dispatch_queue(username, .ls_user_limits(username))
    }
  }
  invisible(NULL)
}

#' @keywords internal
.ls_user_running_jobs <- function(username) {
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(0L)
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  n <- 0L
  for (id in ids) {
    if (isTRUE(.ls_job_consumes_slot(username, id))) {
      n <- n + 1L
    }
  }
  n
}

#' @keywords internal
.ls_user_queued_jobs <- function(username) {
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(0L)
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  n <- 0L
  for (id in ids) {
    meta <- .ls_job_read_meta(username, id)
    if (!is.null(meta) && identical(meta$status, "queued")) {
      n <- n + 1L
    }
  }
  n
}

#' @keywords internal
.ls_user_active_jobs <- function(username) {
  .ls_user_running_jobs(username) + .ls_user_queued_jobs(username)
}

#' @keywords internal
.ls_check_user_limits <- function(username, limits) {
  cfg <- ls_config()
  max_queue <- as.integer(cfg$max_queue %||% 100L)
  queued <- .ls_user_queued_jobs(username)
  if (queued >= max_queue) {
    stop(
      "Job queue full (", queued, "/", max_queue, ").",
      call. = FALSE
    )
  }
  used_mb <- .ls_dir_size_mb(.ls_user_sandbox(username))
  max_disk <- as.integer(limits$max_disk_mb %||% 5120L)
  if (used_mb >= max_disk) {
    stop(
      "Disk quota exceeded (", round(used_mb, 1), " / ", max_disk, " MB).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
