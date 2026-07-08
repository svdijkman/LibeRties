# Standalone worker bootstrap for callr/Docker (no LibeRties namespace required).
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}

worker_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
}

# Collision-tolerant file I/O (Windows sharing violations). These run before
# LibeRation is loaded, so they cannot rely on the package's own helpers.
.worker_io_retries <- 100L
.worker_io_sleep <- 0.015

worker_append <- function(path, text) {
  if (is.null(path) || !nzchar(path) || is.null(text) || length(text) == 0L) {
    return(invisible(FALSE))
  }
  for (i in seq_len(.worker_io_retries)) {
    con <- suppressWarnings(tryCatch(file(path, open = "a"), error = function(e) NULL))
    if (!is.null(con)) {
      ok <- tryCatch({
        writeLines(text, con, useBytes = TRUE)
        flush(con)
        TRUE
      }, error = function(e) FALSE)
      try(close(con), silent = TRUE)
      if (isTRUE(ok)) {
        return(invisible(TRUE))
      }
    }
    Sys.sleep(.worker_io_sleep)
  }
  invisible(FALSE)
}

worker_write_lines <- function(path, text) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }
  for (i in seq_len(.worker_io_retries)) {
    con <- suppressWarnings(tryCatch(file(path, open = "w"), error = function(e) NULL))
    if (!is.null(con)) {
      ok <- tryCatch({
        writeLines(as.character(text), con, useBytes = TRUE)
        flush(con)
        TRUE
      }, error = function(e) FALSE)
      try(close(con), silent = TRUE)
      if (isTRUE(ok)) {
        return(invisible(TRUE))
      }
    }
    Sys.sleep(.worker_io_sleep)
  }
  invisible(FALSE)
}

worker_read_rds <- function(path, default = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(default)
  }
  sentinel <- structure(list(), class = ".worker_io_fail")
  for (i in seq_len(.worker_io_retries)) {
    res <- suppressWarnings(tryCatch(readRDS(path), error = function(e) sentinel))
    if (!inherits(res, ".worker_io_fail")) {
      return(res)
    }
    Sys.sleep(.worker_io_sleep)
  }
  default
}

worker_save_rds <- function(obj, path) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }
  for (i in seq_len(.worker_io_retries)) {
    ok <- suppressWarnings(tryCatch({
      saveRDS(obj, path)
      TRUE
    }, error = function(e) FALSE))
    if (isTRUE(ok)) {
      return(invisible(TRUE))
    }
    Sys.sleep(.worker_io_sleep)
  }
  invisible(FALSE)
}

worker_is_dev_root <- function(root) {
  if (is.null(root) || !nzchar(root) || !dir.exists(root)) {
    return(FALSE)
  }
  if (file.exists(file.path(root, "Meta", "package.rds"))) {
    return(FALSE)
  }
  file.exists(file.path(root, "DESCRIPTION")) && dir.exists(file.path(root, "R"))
}

worker_record_error <- function(job_path, msg) {
  msg <- as.character(msg)[1L]
  if (is.na(msg) || !nzchar(msg)) {
    msg <- "Unknown worker error."
  }
  log_path <- file.path(job_path, "worker.log")
  worker_append(log_path, paste0("Worker error: ", msg))
  err_path <- file.path(job_path, "error.txt")
  if (!file.exists(err_path)) {
    worker_write_lines(err_path, msg)
  }
  meta_path <- file.path(job_path, "meta.rds")
  if (file.exists(meta_path)) {
    meta <- worker_read_rds(meta_path)
    if (!is.null(meta)) {
      meta$status <- "error"
      meta$error <- msg
      if (is.null(meta$finished) || !nzchar(meta$finished)) {
        meta$finished <- worker_now()
      }
      worker_save_rds(meta, meta_path)
    }
  }
  invisible(msg)
}

worker_touch_job <- function(job_path) {
  worker_write_lines(file.path(job_path, "worker.pid"), as.character(Sys.getpid()))
  worker_write_lines(file.path(job_path, "worker.heartbeat"), as.character(Sys.time()))
}

worker_sandbox_root <- function(job_path) {
  env <- Sys.getenv("LIBERTIES_SANDBOX_ROOT", "")
  if (nzchar(env)) {
    return(env)
  }
  normalizePath(file.path(job_path, "..", "..", ".."), winslash = "/", mustWork = FALSE)
}

worker_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0L) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    out <- tryCatch(
      shell(sprintf('tasklist /FI "PID eq %d"', pid), intern = TRUE, mustWork = FALSE),
      error = function(e) character()
    )
    return(any(grepl(as.character(pid), out, fixed = TRUE)))
  }
  identical(suppressWarnings(system2("ps", c("-p", as.character(pid)),
                                     stdout = FALSE, stderr = FALSE)), 0L)
}

worker_lock_is_stale <- function(lock_path) {
  if (!file.exists(lock_path)) {
    return(TRUE)
  }
  lines <- character(0)
  for (i in seq_len(20L)) {
    lines <- suppressWarnings(tryCatch(readLines(lock_path, warn = FALSE), error = function(e) character(0)))
    if (length(lines) >= 2L) {
      break
    }
    Sys.sleep(0.02)
  }
  if (length(lines) < 2L) {
    # Ambiguous (partial/failed read): do not steal a possibly-live lock.
    return(FALSE)
  }
  pid <- suppressWarnings(as.integer(lines[[2L]]))
  if (!is.na(pid) && pid > 0L && !worker_pid_alive(pid)) {
    return(TRUE)
  }
  if (length(lines) >= 3L) {
    ts <- suppressWarnings(as.POSIXct(lines[[3L]], tz = ""))
    if (!is.na(ts) && difftime(Sys.time(), ts, units = "hours") > 6) {
      return(TRUE)
    }
  }
  FALSE
}

worker_acquire_lock <- function(job_path, log_path = NULL) {
  sandbox <- worker_sandbox_root(job_path)
  lock_path <- file.path(sandbox, ".worker_native.lock")
  log_wait <- function(msg) {
    if (!is.null(log_path)) {
      worker_append(log_path, msg)
    }
    worker_touch_job(job_path)
  }
  repeat {
    if (worker_lock_is_stale(lock_path)) {
      tryCatch(unlink(lock_path), error = function(e) NULL)
    }
    ok <- tryCatch({
      con <- file(lock_path, open = "wx")
      writeLines(
        c(basename(job_path), as.character(Sys.getpid()), as.character(Sys.time())),
        con
      )
      close(con)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      return(invisible(lock_path))
    }
    if (!file.exists(lock_path)) {
      next
    }
    holder <- suppressWarnings(tryCatch(readLines(lock_path, warn = FALSE), error = function(e) character(0)))
    holder_name <- if (length(holder) >= 1L) holder[[1L]] else "another job"
    log_wait(paste0(
      "Waiting for native worker lock (held by ", holder_name, ")..."
    ))
    Sys.sleep(2)
  }
}

worker_release_lock <- function(lock_path) {
  if (is.null(lock_path) || !nzchar(lock_path)) {
    return(invisible(FALSE))
  }
  tryCatch(unlink(lock_path), error = function(e) NULL)
  invisible(TRUE)
}

worker_load_pkg_root <- function(root, pkg, log_path = NULL, job_path = NULL) {
  log <- function(...) {
    if (!is.null(log_path)) {
      worker_append(log_path, paste0(...))
    }
    if (!is.null(job_path)) {
      worker_write_lines(file.path(job_path, "worker.heartbeat"), as.character(Sys.time()))
    }
  }
  if (is.null(root) || !nzchar(root)) {
    return(invisible(FALSE))
  }
  if (worker_is_dev_root(root)) {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
      stop("Package 'pkgload' is required to run jobs from development package sources.", call. = FALSE)
    }
    log("Loading dev ", pkg, " from: ", root, "\n")
    pkgload::load_all(root, quiet = TRUE, compile = FALSE, recompile = FALSE)
    return(invisible(TRUE))
  }
  if (requireNamespace(pkg, quietly = TRUE)) {
    log("Loading installed ", pkg, "\n")
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    return(invisible(TRUE))
  }
  stop("Cannot load ", pkg, ".", call. = FALSE)
}

worker_load_packages <- function(dev_env, log_path = NULL, job_path = NULL) {
  log <- function(...) {
    if (!is.null(log_path)) {
      worker_append(log_path, paste0(...))
    }
    if (!is.null(job_path)) {
      worker_write_lines(file.path(job_path, "worker.heartbeat"), as.character(Sys.time()))
    }
  }
  log("R ", R.version.string, "\n")
  log(".libPaths: ", paste(.libPaths(), collapse = "; "), "\n")
  if (identical(dev_env$mode, "dev") && nzchar(dev_env$nm_root %||% "")) {
    worker_load_pkg_root(dev_env$ad_root %||% "", "LibeRtAD", log_path, job_path)
    worker_load_pkg_root(dev_env$nm_root, "LibeRation", log_path, job_path)
    log("LibeRation dev path: ", dev_env$nm_root, "\n")
    return(invisible(TRUE))
  }
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    stop(
      "Package LibeRation is not installed and no development source path is configured.",
      call. = FALSE
    )
  }
  log("Loading installed package LibeRation (", system.file("", package = "LibeRation"), ")\n")
  tryCatch(
    suppressPackageStartupMessages(library(LibeRation)),
    error = function(e) {
      stop("Failed to load LibeRation: ", conditionMessage(e), call. = FALSE)
    }
  )
  log("LibeRtAD path: ", system.file("", package = "LibeRtAD"), "\n")
  invisible(TRUE)
}

run_job_worker <- function(job_path) {
  log_path <- file.path(job_path, "worker.log")
  # Read DEK from a short-lived key file if provided (preferred over env var).
  key_path <- Sys.getenv("LIBERTIES_JOB_KEY_PATH", "")
  if (nzchar(key_path) && file.exists(key_path)) {
    dek_line <- suppressWarnings(tryCatch(
      readLines(key_path, n = 1L, warn = FALSE),
      error = function(e) character(0)
    ))
    if (length(dek_line) > 0L && nzchar(dek_line[[1L]])) {
      Sys.setenv(LIBERTIES_JOB_DEK = dek_line[[1L]])
    }
    tryCatch(unlink(key_path), error = function(e) NULL)
  }
  worker_touch_job(job_path)
  env_path <- file.path(job_path, "env.rds")
  dev_env <- if (file.exists(env_path)) {
    worker_read_rds(env_path, default = list(mode = "installed", nm_root = "", ad_root = ""))
  } else {
    list(mode = "installed", nm_root = "", ad_root = "")
  }
  # Only serialize native workers behind the global lock when the server asked
  # for it. With serialization off, jobs run concurrently up to each user's
  # max_concurrent_jobs (and the server-wide max_global_running cap).
  lock_path <- NULL
  if (isTRUE(dev_env$serial_native)) {
    lock_path <- worker_acquire_lock(job_path, log_path = log_path)
    on.exit(worker_release_lock(lock_path), add = TRUE)
  }
  tryCatch({
    worker_load_packages(dev_env, log_path = log_path, job_path = job_path)
    LibeRation:::.nm_job_worker_impl(job_path)
  }, error = function(e) {
    worker_record_error(job_path, conditionMessage(e))
  }, finally = {
    stderr_path <- file.path(job_path, "worker.stderr")
    if (file.exists(stderr_path)) {
      se <- suppressWarnings(tryCatch(readLines(stderr_path, warn = FALSE), error = function(e) character(0)))
      if (length(se) > 0L) {
        worker_append(log_path, paste0("--- worker stderr ---\n", paste(se, collapse = "\n")))
      }
    }
  })
  invisible(NULL)
}
