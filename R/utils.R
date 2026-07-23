`%||%` <- function(x, y) if (is.null(x)) y else x

.ls_stop <- function(..., call. = FALSE) stop(..., call. = call.)

.ls_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

.ls_safe_component <- function(x, what = "path component") {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(x) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]{0,127}$", x) ||
      x %in% c(".", "..")) {
    .ls_stop("Invalid ", what, ": use 1-128 ASCII letters, digits, '.', '_', or '-'.")
  }
  x
}

.ls_default_root <- function() {
  configured <- Sys.getenv("LIBERTIES_ROOT", unset = "")
  if (nzchar(configured)) return(path.expand(configured))
  configured <- getOption("LibeRties.root", "")
  if (length(configured) == 1L && !is.na(configured) && nzchar(configured)) {
    return(path.expand(configured))
  }
  file.path(tools::R_user_dir("LibeRties", "data"), "queue")
}

.ls_ensure_dir <- function(path) {
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    .ls_stop("Unable to create directory: ", path)
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (.Platform$OS.type != "windows") Sys.chmod(normalized, mode = "0700")
  normalized
}

.ls_worker_env <- function(job_dir) {
  keep <- c("PATH", "SystemRoot", "WINDIR", "TEMP", "TMP", "TMPDIR",
            "HOME", "USERPROFILE", "R_LIBS_USER", "R_LIBS_SITE")
  current <- Sys.getenv(keep, unset = NA_character_)
  current <- current[!is.na(current) & nzchar(current)]
  c(
    current, R_ENVIRON_USER = "", R_PROFILE_USER = "", R_HISTFILE = "",
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
    LIBER_JOB_DIR = normalizePath(job_dir, winslash = "/", mustWork = TRUE)
  )
}

.ls_resource_usage <- function(pid) {
  handle <- ps::ps_handle(as.integer(pid))
  descendants <- tryCatch(ps::ps_children(handle, recursive = TRUE),
                          error = function(error) list())
  handles <- c(list(handle), descendants)
  alive <- vapply(handles, function(value) {
    isTRUE(tryCatch(ps::ps_is_running(value), error = function(error) FALSE))
  }, logical(1))
  handles <- handles[alive]
  memory <- sum(vapply(handles, function(value) {
    tryCatch(as.numeric(ps::ps_memory_info(value)[["rss"]]), error = function(error) 0)
  }, numeric(1)), na.rm = TRUE)
  cpu <- sum(vapply(handles, function(value) {
    times <- tryCatch(ps::ps_cpu_times(value), error = function(error) NULL)
    if (is.null(times)) 0 else sum(as.numeric(times[c("user", "system")]), na.rm = TRUE)
  }, numeric(1)), na.rm = TRUE)
  list(
    memory_mb = memory / 1024^2,
    cpu_seconds = cpu,
    processes = length(handles)
  )
}

.ls_kill_process_tree <- function(pid) {
  handle <- tryCatch(ps::ps_handle(as.integer(pid)), error = function(error) NULL)
  if (is.null(handle)) return(invisible(FALSE))
  descendants <- tryCatch(ps::ps_children(handle, recursive = TRUE),
                          error = function(error) list())
  for (child in rev(descendants)) try(ps::ps_kill(child), silent = TRUE)
  try(ps::ps_kill(handle), silent = TRUE)
  invisible(TRUE)
}

.ls_job_dir <- function(root, user, id) {
  user <- .ls_safe_component(user, "user id")
  id <- .ls_safe_component(id, "job id")
  root <- .ls_ensure_dir(root)
  candidate <- file.path(root, "users", user, "jobs", id)
  parent <- .ls_ensure_dir(dirname(candidate))
  path <- file.path(parent, basename(candidate))
  root_cmp <- if (.Platform$OS.type == "windows") tolower(root) else root
  path_cmp <- if (.Platform$OS.type == "windows") tolower(path) else path
  prefix <- paste0(root_cmp, "/")
  if (!startsWith(path_cmp, prefix)) .ls_stop("Resolved job path escaped the queue root.")
  path
}

.ls_storage_key <- function(required = FALSE) {
  encoded <- Sys.getenv("LIBERTIES_STORAGE_KEY", unset = "")
  if (!nzchar(encoded)) encoded <- getOption("LibeRties.storage_key", "")
  encoded <- trimws(as.character(encoded %||% ""))
  if (!nzchar(encoded)) {
    if (isTRUE(required)) .ls_stop("LIBERTIES_STORAGE_KEY is required for encrypted storage.")
    return(NULL)
  }
  if (length(encoded) != 1L || !grepl("^[A-Fa-f0-9]{64}$", encoded)) {
    .ls_stop("LIBERTIES_STORAGE_KEY must be exactly 64 hexadecimal characters (256 bits).")
  }
  bytes <- substring(encoded, seq.int(1L, 63L, by = 2L), seq.int(2L, 64L, by = 2L))
  as.raw(strtoi(bytes, base = 16L))
}

.ls_storage_wrap <- function(object) {
  key <- .ls_storage_key()
  if (is.null(key)) return(object)
  list(
    schema = "liberties.encrypted-rds", version = 1L,
    key_id = substr(.ls_token_hash(paste(sprintf("%02x", as.integer(key)), collapse = "")), 1L, 16L),
    payload = sodium::data_encrypt(serialize(object, NULL, version = 3L), key)
  )
}

.ls_storage_unwrap <- function(object) {
  if (!is.list(object) || !identical(object$schema %||% "", "liberties.encrypted-rds")) {
    return(object)
  }
  if (!identical(as.integer(object$version), 1L) || !is.raw(object$payload)) {
    .ls_stop("Encrypted LibeRties record is malformed.")
  }
  key <- .ls_storage_key(required = TRUE)
  tryCatch(
    unserialize(sodium::data_decrypt(object$payload, key)),
    error = function(error) .ls_stop("Unable to authenticate or decrypt LibeRties storage.")
  )
}

.ls_atomic_save_rds <- function(object, path) {
  dir <- .ls_ensure_dir(dirname(path))
  tmp <- tempfile("write-", tmpdir = dir, fileext = ".rds")
  backup <- paste0(path, ".previous")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  saveRDS(.ls_storage_wrap(object), tmp, version = 3)
  if (file.exists(path)) {
    unlink(backup, force = TRUE)
    if (!file.rename(path, backup)) .ls_stop("Unable to rotate metadata file: ", path)
  }
  if (!file.rename(tmp, path)) {
    if (file.exists(backup)) file.rename(backup, path)
    .ls_stop("Unable to publish file: ", path)
  }
  if (.Platform$OS.type != "windows") Sys.chmod(path, mode = "0600")
  unlink(backup, force = TRUE)
  invisible(path)
}

.ls_read_rds <- function(path, attempts = 4L) {
  last <- NULL
  for (i in seq_len(attempts)) {
    candidate <- if (file.exists(path)) path else if (file.exists(paste0(path, ".previous"))) {
      paste0(path, ".previous")
    } else path
    value <- tryCatch(.ls_storage_unwrap(suppressWarnings(readRDS(candidate))), error = function(e) {
      last <<- e
      NULL
    })
    if (!is.null(value)) return(value)
    if (i < attempts) Sys.sleep(0.01)
  }
  .ls_stop("Unable to read ", path, ": ", conditionMessage(last))
}

.ls_meta_path <- function(job_dir) file.path(job_dir, "metadata.rds")
.ls_payload_path <- function(job_dir) file.path(job_dir, "payload.rds")
.ls_result_path <- function(job_dir) file.path(job_dir, "result.rds")
.ls_log_path <- function(job_dir, stream) file.path(job_dir, paste0(stream, ".log"))
.ls_log_archive_path <- function(job_dir, stream) file.path(job_dir, paste0(stream, ".log.rds"))

.ls_seal_job_logs <- function(job_dir) {
  if (is.null(.ls_storage_key())) return(invisible(FALSE))
  changed <- FALSE
  for (stream in c("stdout", "stderr")) {
    path <- .ls_log_path(job_dir, stream)
    archive <- .ls_log_archive_path(job_dir, stream)
    if (!file.exists(path)) next
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    .ls_atomic_save_rds(lines, archive)
    verified <- .ls_read_rds(archive)
    if (!identical(as.character(verified), as.character(lines))) {
      .ls_stop("Unable to verify encrypted ", stream, " log archive.")
    }
    unlink(path, force = TRUE)
    changed <- TRUE
  }
  error_path <- file.path(job_dir, "error.txt")
  if (file.exists(error_path)) {
    error <- readLines(error_path, warn = FALSE, encoding = "UTF-8")
    .ls_atomic_save_rds(error, file.path(job_dir, "error.txt.rds"))
    unlink(error_path, force = TRUE)
    changed <- TRUE
  }
  invisible(changed)
}

.ls_read_job_log <- function(job_dir, stream) {
  archive <- .ls_log_archive_path(job_dir, stream)
  if (file.exists(archive)) return(as.character(.ls_read_rds(archive)))
  path <- .ls_log_path(job_dir, stream)
  if (!file.exists(path)) return(character())
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

.ls_read_meta <- function(job_dir) .ls_read_rds(.ls_meta_path(job_dir))
.ls_write_meta <- function(job_dir, metadata) {
  metadata$updated <- .ls_now()
  .ls_atomic_save_rds(metadata, .ls_meta_path(job_dir))
}

.ls_with_job_lock <- function(job_dir, operation, timeout = 5, stale_after = 30) {
  lock <- file.path(job_dir, ".metadata.lock")
  started <- proc.time()[["elapsed"]]
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) break
    age <- tryCatch(as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "secs")),
                    error = function(e) 0)
    if (is.finite(age) && age > stale_after) {
      unlink(lock, recursive = TRUE, force = TRUE)
      next
    }
    if (proc.time()[["elapsed"]] - started >= timeout) {
      .ls_stop("Timed out acquiring job metadata lock: ", basename(job_dir), ".")
    }
    Sys.sleep(0.01)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  operation()
}

.ls_update_meta <- function(job_dir, update, allowed_status = NULL) {
  .ls_with_job_lock(job_dir, function() {
    metadata <- .ls_read_meta(job_dir)
    if (!is.null(allowed_status) && !metadata$status %in% allowed_status) return(metadata)
    for (name in names(update)) metadata[[name]] <- update[[name]]
    .ls_write_meta(job_dir, metadata)
    metadata
  })
}

.ls_md5 <- function(path) unname(tools::md5sum(path)[[1L]])

.ls_sha256 <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  unname(paste0(openssl::sha256(connection)))
}

.ls_digest_matches <- function(path, metadata, prefix) {
  sha_name <- paste0(prefix, "_sha256")
  md5_name <- paste0(prefix, "_md5")
  expected_sha <- as.character(metadata[[sha_name]] %||% "")
  if (nzchar(expected_sha)) return(identical(.ls_sha256(path), expected_sha))
  expected_md5 <- as.character(metadata[[md5_name]] %||% "")
  nzchar(expected_md5) && identical(.ls_md5(path), expected_md5)
}

.ls_random_hex <- function(bytes) {
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(as.integer(bytes)))), collapse = "")
}

.ls_new_id <- function() {
  paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-", .ls_random_hex(16L))
}

.ls_terminal <- function(status) status %in% c("completed", "failed", "cancelled")

.ls_empty_jobs <- function() {
  data.frame(
    id = character(), user = character(), type = character(), label = character(),
    status = character(), submitted = character(), started = character(),
    finished = character(), stringsAsFactors = FALSE
  )
}
