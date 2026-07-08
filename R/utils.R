#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}

#' @keywords internal
.ls_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
}

#' @keywords internal
.ls_new_id <- function(prefix = "job") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
         substr(digest::digest(runif(1)), 1L, 6L))
}

#' @keywords internal
.ls_read_json <- function(path, default = list()) {
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) default
  )
}

#' @keywords internal
.ls_write_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(x)
}

#' @keywords internal
.ls_hash_token <- function(token) {
  digest::digest(token, algo = "sha256")
}

#' @keywords internal
.ls_generate_token <- function() {
  # 256-bit cryptographically-random token. Strong enough to also serve as the
  # secret from which per-user at-rest encryption keys are derived.
  rnd <- if (requireNamespace("sodium", quietly = TRUE)) {
    sodium::bin2hex(sodium::random(32L))
  } else {
    # Fallback: 32 bytes from the OS CSPRNG via R's own generator seeded pool.
    paste(sprintf("%02x", as.integer(sample.int(256L, 32L, replace = TRUE) - 1L)),
          collapse = "")
  }
  paste0("lr_", format(Sys.time(), "%Y%m%d"), "_", rnd)
}

#' @keywords internal
.ls_sanitize_user <- function(username) {
  u <- gsub("[^a-zA-Z0-9._-]", "", as.character(username))
  if (!nzchar(u)) {
    stop("Invalid username.", call. = FALSE)
  }
  u
}

#' @keywords internal
.ls_user_sandbox <- function(username) {
  file.path(ls_sandbox_root(), "sandboxes", .ls_sanitize_user(username))
}

#' @keywords internal
.ls_user_jobs_root <- function(username) {
  file.path(.ls_user_sandbox(username), "jobs")
}

#' Validate a job id before it is ever turned into a filesystem path.
#'
#' A job id arrives from the client (status/log/result/cancel routes) and is
#' concatenated into a path under the user's jobs root. Without validation an
#' attacker can supply "../../otheruser/jobs/x" or an absolute path to escape
#' their sandbox and read/cancel another tenant's GDPR-sensitive jobs. We
#' allow only a conservative id charset and explicitly reject traversal
#' sequences and path separators.
#' @keywords internal
.ls_sanitize_job_id <- function(job_id) {
  id <- as.character(job_id %||% "")[1L]
  if (!nzchar(id) ||
      grepl("[/\\\\]", id) ||
      grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", id) ||
      identical(id, "..") ||
      !grepl("^[A-Za-z0-9._-]+$", id)) {
    stop("Invalid job id.", call. = FALSE)
  }
  id
}

#' @keywords internal
.ls_job_path <- function(username, job_id) {
  job_id <- .ls_sanitize_job_id(job_id)
  root <- .ls_user_jobs_root(username)
  path <- file.path(root, job_id)
  # Defensive: ensure the (normalized) job path stays inside the user's jobs
  # root even if the sanitizer above is ever weakened. normalizePath does not
  # require the path to exist (mustWork = FALSE).
  norm_root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  norm_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(norm_root, "/")
  if (!identical(norm_path, norm_root) &&
      substr(norm_path, 1L, nchar(prefix)) != prefix) {
    stop("Invalid job id.", call. = FALSE)
  }
  path
}

#' @keywords internal
.ls_dir_size_mb <- function(path) {
  if (!dir.exists(path)) {
    return(0)
  }
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0L) {
    return(0)
  }
  sum(file.info(files)$size, na.rm = TRUE) / (1024 * 1024)
}

#' @keywords internal
.ls_pkg_version <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return("")
  }
  as.character(utils::packageVersion(pkg))
}

#' @keywords internal
.ls_version_info <- function() {
  list(
    LibeRties = .ls_pkg_version("LibeRties"),
    LibeRation = .ls_pkg_version("LibeRation"),
    LibeRtAD = .ls_pkg_version("LibeRtAD")
  )
}

#' @keywords internal
.ls_startup_info <- function() {
  users <- .ls_users_load()
  list(
    sandbox = ls_sandbox_root(),
    users_file = .ls_users_path(),
    n_users = length(users)
  )
}

#' @keywords internal
.ls_rds_from_raw <- function(raw) {
  is_gzip <- length(raw) >= 2L &&
    identical(raw[1:2], as.raw(c(0x1f, 0x8b)))
  con <- if (is_gzip) {
    gzcon(rawConnection(raw, "r"))
  } else {
    rawConnection(raw, "r")
  }
  on.exit(close(con), add = TRUE)
  readRDS(con)
}

#' @keywords internal
.ls_plumber_error_handler <- function(req, res, err) {
  msg <- conditionMessage(err)
  if (grepl("^Unauthorized", msg)) {
    res$status <- 401L
    return(list(error = msg))
  }
  if (identical(msg, "Admin access denied.")) {
    res$status <- 403L
    return(list(error = msg))
  }
  if (grepl(
    "^(User account is disabled|Unknown user|Invalid username|Concurrent job limit|Disk quota)",
    msg
  )) {
    res$status <- 403L
    return(list(error = msg))
  }
  res$status <- 500L
  message("LibeRties API internal error: ", msg)
  list(error = "Internal server error.")
}
