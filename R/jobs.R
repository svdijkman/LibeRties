#' @keywords internal
.ls_worker_script <- function() {
  system.file("worker", "run_job.R", package = "LibeRties")
}

# ---------------------------------------------------------------------------
# Collision-tolerant file I/O. On Windows, a status poll reading meta.rds /
# worker.log while the worker writes the same file hits a sharing violation
# ("cannot open the connection" / "Permission denied"). Retry briefly so a
# transient collision never crashes a worker or mislabels a job.
# ---------------------------------------------------------------------------

#' @keywords internal
.ls_io_retry_max <- function() {
  as.integer(getOption("LibeRties.io_retry_max", 100L))
}

#' @keywords internal
.ls_io_retry_sleep <- function() {
  as.numeric(getOption("LibeRties.io_retry_sleep", 0.015))
}

#' @keywords internal
.ls_read_rds_safe <- function(path, default = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(default)
  }
  sentinel <- structure(list(), class = ".ls_io_fail")
  for (i in seq_len(.ls_io_retry_max())) {
    res <- suppressWarnings(tryCatch(readRDS(path), error = function(e) sentinel))
    if (!inherits(res, ".ls_io_fail")) {
      return(res)
    }
    Sys.sleep(.ls_io_retry_sleep())
  }
  default
}

#' @keywords internal
.ls_save_rds_safe <- function(obj, path) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }
  retries <- .ls_io_retry_max()
  sleep <- .ls_io_retry_sleep()
  for (i in seq_len(retries)) {
    ok <- suppressWarnings(tryCatch({
      saveRDS(obj, path)
      TRUE
    }, error = function(e) FALSE))
    if (isTRUE(ok)) {
      return(invisible(TRUE))
    }
    Sys.sleep(sleep)
  }
  invisible(FALSE)
}

#' Persist job meta.rds; warn (non-fatal) if the write fails so status is not
#' silently desynced from on-disk reality.
#' @keywords internal
.ls_save_job_meta <- function(meta, job_path) {
  path <- file.path(job_path, "meta.rds")
  ok <- isTRUE(.ls_save_rds_safe(meta, path))
  if (!ok) {
    warning(
      "Failed to persist job metadata to ", path,
      "; job state may be stale until the next successful write.",
      call. = FALSE
    )
  }
  invisible(ok)
}

#' @keywords internal
.ls_est_limit_cfg <- function(cfg) {
  cfg <- cfg %||% ls_config()
  list(
    maxit = as.integer(cfg$max_est_maxit %||% 5000L),
    max_outer = as.integer(cfg$max_est_max_outer %||% 50L),
    n_iter = as.integer(cfg$max_est_n_iter %||% 10000L),
    n_burn = as.integer(cfg$max_est_n_burn %||% 5000L),
    n_mcmc = as.integer(cfg$max_est_n_mcmc %||% 50000L),
    n_quad = as.integer(cfg$max_est_n_quad %||% 20L),
    n_imp = as.integer(cfg$max_est_n_imp %||% 5000L),
    bootstrap_n = as.integer(cfg$max_est_bootstrap_n %||% 500L)
  )
}

#' Reject client-supplied estimation knobs that exceed server-side maxima.
#' @keywords internal
.ls_enforce_est_limits <- function(est_args, cfg = NULL) {
  lim <- .ls_est_limit_cfg(cfg)
  ctl <- est_args$control %||% list()
  if (!is.null(ctl$maxit) && as.integer(ctl$maxit[1L]) > lim$maxit) {
    stop("control$maxit exceeds server maximum (", lim$maxit, ").", call. = FALSE)
  }
  for (nm in c("max_outer", "n_iter", "n_burn", "n_mcmc", "n_quad", "n_imp", "bootstrap_n")) {
    val <- est_args[[nm]]
    if (!is.null(val) && as.integer(val[1L]) > lim[[nm]]) {
      stop(nm, " exceeds server maximum (", lim[[nm]], ").", call. = FALSE)
    }
  }
  invisible(est_args)
}

#' Write the per-job DEK to a short-lived key file (0600) and return its path.
#' The path (not the secret) is passed to the worker via LIBERTIES_JOB_KEY_PATH.
#' @keywords internal
.ls_write_job_dek_file <- function(job_path, dek_hex) {
  if (!is.character(dek_hex) || length(dek_hex) != 1L || !nzchar(dek_hex)) {
    return("")
  }
  key_path <- file.path(job_path, ".dek.key")
  writeLines(dek_hex, key_path, useBytes = TRUE)
  if (.Platform$OS.type == "windows") {
    tryCatch(
      system2("icacls", c(key_path, "/inheritance:r", "/grant:r",
                          paste0(Sys.info()[["user"]], ":(R)")), stdout = FALSE, stderr = FALSE),
      error = function(e) NULL
    )
  } else {
    tryCatch(Sys.chmod(key_path, mode = "0600"), error = function(e) NULL)
  }
  key_path
}

#' Remove stale result/error artefacts before relaunching a queued job.
#' @keywords internal
.ls_clear_stale_job_artifacts <- function(job_path) {
  for (nm in c("result.rds", "result.enc", "error.txt")) {
    p <- file.path(job_path, nm)
    if (file.exists(p)) {
      unlink(p)
    }
  }
  invisible(NULL)
}

#' Latest mtime among result files, or NA if none.
#' @keywords internal
.ls_job_result_mtime <- function(job_path) {
  paths <- c(
    file.path(job_path, "result.rds"),
    file.path(job_path, "result.enc")
  )
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0L) {
    return(as.POSIXct(NA))
  }
  max(file.info(paths)$mtime, na.rm = TRUE)
}

#' TRUE when a result file is consistent with the current launch (not stale).
#' @keywords internal
.ls_job_result_trustworthy <- function(meta, job_path) {
  if (!.ls_job_has_result(job_path)) {
    return(FALSE)
  }
  error_path <- file.path(job_path, "error.txt")
  if (file.exists(error_path)) {
    err_info <- file.info(error_path)
    if (!is.na(err_info$size) && err_info$size > 0L) {
      res_mtime <- .ls_job_result_mtime(job_path)
      if (!is.na(res_mtime) && err_info$mtime >= res_mtime) {
        return(FALSE)
      }
    }
  }
  started <- meta$started %||% ""
  if (!nzchar(started)) {
    return(TRUE)
  }
  started_ts <- suppressWarnings(as.POSIXct(started, tz = ""))
  res_mtime <- .ls_job_result_mtime(job_path)
  if (is.na(started_ts) || is.na(res_mtime)) {
    return(TRUE)
  }
  res_mtime >= started_ts - 1
}

#' @keywords internal
.ls_read_lines_safe <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(character(0))
  }
  sentinel <- structure(list(), class = ".ls_io_fail")
  for (i in seq_len(.ls_io_retry_max())) {
    res <- suppressWarnings(tryCatch(readLines(path, warn = FALSE), error = function(e) sentinel))
    if (!inherits(res, ".ls_io_fail")) {
      return(res)
    }
    Sys.sleep(.ls_io_retry_sleep())
  }
  character(0)
}

# ---------------------------------------------------------------------------
# Live worker process registry.
#
# callr/processx launch workers with cleanup = TRUE, whose finalizer calls
# TerminateProcess on the child when the R process handle is garbage-collected.
# The long-running API only kept the handle as a local variable plus a
# serialized on-disk copy, so after dispatch returned there was no strong
# in-memory reference. The next GC then silently killed a perfectly healthy
# worker, producing empty stdout/stderr, a frozen log, and the dreaded
# "Worker exited without result." error.
#
# Keeping a strong reference for the lifetime of the job prevents the finalizer
# from running until the worker has already exited (when killing is a no-op).
# ---------------------------------------------------------------------------

.ls_proc_registry <- new.env(parent = emptyenv())

#' @keywords internal
.ls_proc_register <- function(job_id, proc) {
  if (is.null(proc) || is.null(job_id) || !nzchar(job_id)) {
    return(invisible(NULL))
  }
  assign(job_id, proc, envir = .ls_proc_registry)
  invisible(NULL)
}

#' @keywords internal
.ls_proc_unregister <- function(job_id) {
  if (is.null(job_id) || !nzchar(job_id)) {
    return(invisible(NULL))
  }
  if (exists(job_id, envir = .ls_proc_registry, inherits = FALSE)) {
    rm(list = job_id, envir = .ls_proc_registry)
  }
  invisible(NULL)
}

#' Drop registry entries whose worker process has already exited. Safe because
#' a dead process cannot be killed by a later finalizer.
#' @keywords internal
.ls_proc_sweep <- function() {
  ids <- ls(.ls_proc_registry, all.names = TRUE)
  for (id in ids) {
    proc <- tryCatch(get(id, envir = .ls_proc_registry, inherits = FALSE),
                     error = function(e) NULL)
    alive <- tryCatch(!is.null(proc) && isTRUE(proc$is_alive()), error = function(e) FALSE)
    if (!isTRUE(alive)) {
      rm(list = id, envir = .ls_proc_registry)
    }
  }
  invisible(NULL)
}

#' @keywords internal
.ls_launch_job <- function(username, job_id, limits, dek_hex = "") {
  cfg <- ls_config()
  job_path <- .ls_job_path(username, job_id)
  launcher <- cfg$launcher %||% "local"

  if (identical(launcher, "docker")) {
    return(.ls_launch_docker(job_path, limits, cfg, dek_hex = dek_hex))
  }
  .ls_launch_local(job_path, limits, cfg, dek_hex = dek_hex)
}

#' @keywords internal
.ls_job_normalize_est_args <- function(est_args, method, model = NULL) {
  if (is.null(est_args)) {
    est_args <- list()
  }
  if (is.data.frame(est_args)) {
    est_args <- as.list(est_args)
  }
  ctl <- est_args$control
  if (is.null(ctl)) {
    ctl <- list()
  }
  if (is.data.frame(ctl)) {
    ctl <- as.list(ctl)
  }
  for (nm in c("maxit", "n_cores", "min_retries")) {
    if (!is.null(ctl[[nm]])) {
      ctl[[nm]] <- as.integer(ctl[[nm]][1L])
    }
  }
  for (nm in c("compute_inference", "tweak_inits", "cov_refit_eta")) {
    if (!is.null(ctl[[nm]])) {
      ctl[[nm]] <- isTRUE(ctl[[nm]])
    }
  }
  if (!is.null(ctl$print_grad_every)) {
    ctl$print_grad_every <- as.integer(ctl$print_grad_every[1L])
  }
  est_args$control <- ctl
  for (nm in c("max_outer", "n_iter", "n_burn", "n_mcmc", "n_quad", "n_imp",
               "bootstrap_n", "bootstrap_seed")) {
    if (!is.null(est_args[[nm]])) {
      est_args[[nm]] <- as.integer(est_args[[nm]][1L])
    }
  }
  method <- toupper(as.character(method %||% "FO")[1L])
  cpp_ok <- is.null(model)
  if (!cpp_ok && requireNamespace("LibeRation", quietly = TRUE)) {
    cpp_ok <- isTRUE(LibeRation:::.nm_cpp_capable(model))
  }
  if (method %in% c("FOCE", "LAPLACE") && cpp_ok) {
    est_args$pk_engine <- "cpp"
    est_args$grad <- "cpp"
  }
  if (method == "FOCEI" && cpp_ok) {
    est_args$pk_engine <- "cpp"
    if (is.null(est_args$grad) || est_args$grad %in% c("cpp", "numeric")) {
      est_args$grad <- "auto"
    }
  }
  if (identical(method, "FO") && cpp_ok) {
    est_args$pk_engine <- "cpp"
    if (is.null(est_args$grad) || est_args$grad %in% c("auto", "ad")) {
      est_args$grad <- "cpp"
    }
  }
  if (method == "LAPLACE") {
    est_args$engine <- "cpp"
    if (is.null(est_args$n_quad)) {
      est_args$n_quad <- 3L
    }
  }
  if (method == "FOCEI" && is.null(est_args$max_outer)) {
    est_args$max_outer <- 3L
  }
  if (is.null(est_args$control$compute_inference)) {
    est_args$control$compute_inference <- FALSE
  }
  if (is.null(est_args$control$print_grad_every)) {
    est_args$control$print_grad_every <- 1L
  }
  est_args$control$n_cores <- 1L
  if (method == "SAEM") {
    est_args$engine <- "cpp"
  }
  est_args
}

#' @keywords internal
.ls_job_worker_never_started <- function(meta, job_path) {
  log_path <- file.path(job_path, "worker.log")
  if (file.exists(log_path)) {
    return(FALSE)
  }
  started <- meta$started %||% ""
  if (nzchar(started)) {
    return(FALSE)
  }
  !file.exists(file.path(job_path, "worker.pid")) &&
    !file.exists(file.path(job_path, ".process.rds"))
}

#' @keywords internal
.ls_job_log_loading_phase <- function(job_path, meta = NULL) {
  log_path <- file.path(job_path, "worker.log")
  if (!file.exists(log_path)) {
    if (!is.null(meta)) {
      started <- meta$started %||% ""
      return(nzchar(started))
    }
    return(FALSE)
  }
  lines <- .ls_read_lines_safe(log_path)
  if (length(lines) == 0L) {
    return(TRUE)
  }
  txt <- paste(lines, collapse = "\n")
  if (grepl("Job started:|Running estimation:|Running simulation:", txt)) {
    return(FALSE)
  }
  grepl("Loading dev |Loading installed ", txt, fixed = FALSE)
}

#' @keywords internal
.ls_job_loading_grace_sec <- function() {
  60
}

#' @keywords internal
.ls_dev_null <- function() {
  if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
}

#' Effective concurrent job cap (serializes native LibeRtAD workers when configured).
#' @keywords internal
.ls_effective_max_concurrent <- function(limits, cfg = NULL) {
  if (is.null(cfg)) {
    cfg <- ls_config()
  }
  user <- as.integer(limits$max_concurrent_jobs %||% 1L)
  if (isTRUE(cfg$worker_serial_native %||% FALSE)) {
    return(max(1L, min(user, 1L)))
  }
  max(1L, user)
}

#' Count jobs currently occupying a run slot across ALL users.
#' @keywords internal
.ls_global_running_jobs <- function() {
  root <- file.path(ls_sandbox_root(), "sandboxes")
  if (!dir.exists(root)) {
    return(0L)
  }
  users <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  users <- users[nzchar(users)]
  total <- 0L
  for (u in users) {
    total <- total + .ls_user_running_jobs(u)
  }
  total
}

#' @keywords internal
.ls_launch_local <- function(job_path, limits, cfg, dek_hex = "") {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required for local job launcher.", call. = FALSE)
  }
  bootstrap <- system.file("worker", "bootstrap.R", package = "LibeRties")
  stderr_path <- file.path(job_path, "worker.stderr")
  stdout_path <- file.path(job_path, "worker.stdout")
  worker_env <- c(LIBERTIES_SANDBOX_ROOT = ls_sandbox_root())
  key_path <- ""
  if (is.character(dek_hex) && length(dek_hex) == 1L && nzchar(dek_hex)) {
    # Pass the DEK via a 0600 key file; only the path is visible in the
    # process environment. The worker reads and unlinks the file immediately.
    key_path <- .ls_write_job_dek_file(job_path, dek_hex)
    if (nzchar(key_path)) {
      worker_env <- c(worker_env, LIBERTIES_JOB_KEY_PATH = key_path)
    } else {
      worker_env <- c(worker_env, LIBERTIES_JOB_DEK = dek_hex)
    }
  }
  proc <- callr::r_bg(
    func = function(job_path, bootstrap) {
      if (nzchar(bootstrap) && file.exists(bootstrap)) {
        source(bootstrap, local = TRUE)
        run_job_worker(job_path)
      } else {
        stop("Worker bootstrap script not found.", call. = FALSE)
      }
    },
    args = list(job_path = job_path, bootstrap = bootstrap),
    libpath = .libPaths(),
    repos = getOption("repos"),
    wd = getwd(),
    stdout = stdout_path,
    stderr = stderr_path,
    supervise = TRUE,
    env = worker_env
  )
  pid <- tryCatch(proc$get_pid(), error = function(e) NA_integer_)
  if (is.na(pid)) {
    Sys.sleep(0.2)
    pid <- tryCatch(proc$get_pid(), error = function(e) NA_integer_)
  }
  list(pid = pid, process = proc, mode = "local")
}

#' @keywords internal
.ls_launch_docker <- function(job_path, limits, cfg, dek_hex = "") {
  image <- cfg$docker_image %||% "liberties-worker:latest"
  cpu <- as.integer(limits$max_cpu %||% 4L)
  mem_mb <- as.integer(limits$max_memory_mb %||% 8192L)
  job_path <- normalizePath(job_path, winslash = "/", mustWork = TRUE)
  parent <- dirname(job_path)
  dek_env <- character(0)
  if (is.character(dek_hex) && length(dek_hex) == 1L && nzchar(dek_hex)) {
    key_path <- .ls_write_job_dek_file(job_path, dek_hex)
    if (nzchar(key_path)) {
      dek_env <- c("-e", paste0("LIBERTIES_JOB_KEY_PATH=", key_path))
    } else {
      dek_env <- c("-e", paste0("LIBERTIES_JOB_DEK=", dek_hex))
    }
  }
  args <- c(
    "run", "--rm", "-d",
    "-v", paste0(parent, ":/jobs"),
    "--cpus", as.character(cpu),
    "-m", paste0(mem_mb, "m"),
    "-e", paste0("LIBERTIES_JOB_PATH=/jobs/", basename(job_path)),
    dek_env,
    image,
    "/jobs/", basename(job_path)
  )
  out <- system2("docker", args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status") %||% 0L
  if (!identical(status, 0L)) {
    stop("Docker launch failed: ", paste(out, collapse = "\n"), call. = FALSE)
  }
  container_id <- trimws(out[length(out)])
  list(pid = NA_integer_, container_id = container_id, mode = "docker")
}

# ---------------------------------------------------------------------------
# Untrusted-input validation for submitted model/data objects.
#
# model_b64/data_b64 arrive from the client, are base64-decoded and passed to
# readRDS/unserialize. R's (un)serialize has NO safe mode: a crafted stream can
# execute arbitrary code on load (e.g. via crafted S4/ALTREP/environment
# objects or class methods triggered downstream). There is no way to make
# unserialize of attacker-controlled bytes fully safe in R, so this allowlist
# is a defence-in-depth measure, NOT a complete mitigation.
#
# The REAL mitigation is worker isolation: the deserialization and estimation
# already run in a separate, resource-limited child process (callr / Docker
# launcher). Deploy the Docker launcher (or an OS sandbox / container / seccomp
# profile) on multi-tenant hosts so a malicious payload cannot reach other
# tenants' data even if it executes.
#
# Here we reject anything that is not a plausible model/dataset object as early
# as possible, before it is handed to the worker.
# ---------------------------------------------------------------------------

#' @keywords internal
.ls_validate_submitted_model <- function(model) {
  if (is.null(model) || !inherits(model, "nm_model")) {
    stop(
      "Submitted model is not a valid nm_model object; refusing job.",
      call. = FALSE
    )
  }
  invisible(model)
}

#' @keywords internal
.ls_validate_submitted_data <- function(data) {
  if (is.null(data)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(data) && !inherits(data, "nm_dataset")) {
    stop(
      "Submitted data is not a data.frame/nm_dataset object; refusing job.",
      call. = FALSE
    )
  }
  invisible(data)
}

#' Submit a job for a user
#'
#' @param username Authenticated user.
#' @param limits User limit list.
#' @param payload Parsed JSON body from API.
#' @return Job metadata list.
#' @keywords internal
.ls_job_submit <- function(username, limits, payload) {
  .ls_check_user_limits(username, limits)
  job_type <- payload$job_type %||% "est"
  if (!job_type %in% c("est", "sim")) {
    stop("job_type must be 'est' or 'sim'.", call. = FALSE)
  }

  job_id <- .ls_new_id("job")
  job_path <- .ls_job_path(username, job_id)
  dir.create(job_path, recursive = TRUE, showWarnings = FALSE)

  data <- NULL
  if (!is.null(payload$data_ref)) {
    ref <- payload$data_ref
    ds_id <- ref$dataset_id %||% ref$id
    ds_md5 <- ref$md5 %||% ref$expected_md5
    ds_path <- .ls_dataset_resolve(ds_id, ds_md5, username = username)
    data <- readRDS(ds_path)
  } else if (!is.null(payload$data_b64) && nzchar(payload$data_b64)) {
    raw <- jsonlite::base64_dec(payload$data_b64)
    data <- .ls_rds_from_raw(raw)
  }

  model <- NULL
  if (!is.null(payload$model_b64) && nzchar(payload$model_b64)) {
    raw <- jsonlite::base64_dec(payload$model_b64)
    model <- .ls_rds_from_raw(raw)
  }

  if (is.null(model)) {
    stop("model_b64 is required.", call. = FALSE)
  }
  if (is.null(data) && identical(job_type, "est")) {
    stop("data_ref or data_b64 is required for estimation jobs.", call. = FALSE)
  }

  # Strict allowlist validation of deserialized, attacker-influenced objects
  # BEFORE they are used or passed to the worker (see notes above).
  .ls_validate_submitted_model(model)
  .ls_validate_submitted_data(data)

  est_args <- payload$est_args %||% list()
  if (identical(job_type, "est")) {
    est_args <- .ls_job_normalize_est_args(est_args, payload$method %||% "FO", model)
    .ls_enforce_est_limits(est_args)
    args <- c(
      list(
        model = model,
        data = data,
        method = payload$method %||% "FO"
      ),
      est_args
    )
  } else {
    args <- c(list(model = model, data = data), est_args)
    if (is.null(args$data) && !is.null(payload$data_ref)) {
      args$data <- data
    }
  }

  worker_env <- .ls_job_worker_env()
  cfg <- ls_config()
  worker_env$serial_native <- isTRUE(cfg$worker_serial_native %||% FALSE)
  # Envelope-encrypt the payload (model + patient data) at rest with a per-job
  # DEK wrapped by the owner's key. The plaintext args never touch the disk.
  encrypted <- FALSE
  if (.ls_encryption_enabled(cfg)) {
    uk <- .ls_uk_get(username)
    if (!is.null(uk)) {
      dek <- .ls_dek_generate()
      .ls_encrypt_to_file(args, dek, file.path(job_path, "args.enc"))
      .ls_save_rds_safe(.ls_dek_wrap(dek, uk), file.path(job_path, "key.enc"))
      encrypted <- TRUE
    } else {
      # REFUSE to fall back to plaintext: writing GDPR-sensitive model/patient
      # data unencrypted defeats at-rest encryption. Without the owner's key we
      # cannot envelope-encrypt, so the job cannot be accepted. Remove the
      # partially-created job dir so no empty/plaintext artefacts linger.
      unlink(job_path, recursive = TRUE)
      stop(
        "Job cannot be submitted without the owner's encryption key. ",
        "Re-authenticate with your current API token and retry.",
        call. = FALSE
      )
    }
  } else {
    saveRDS(args, file.path(job_path, "args.rds"))
  }
  saveRDS(worker_env, file.path(job_path, "env.rds"))

  label <- payload$label %||% if (identical(job_type, "sim")) "sim job" else paste(args$method, "fit")
  meta <- list(
    id = job_id,
    user = username,
    label = label,
    job_type = job_type,
    status = "queued",
    method = if (identical(job_type, "est")) args$method else "SIM",
    created = .ls_now(),
    started = "",
    finished = "",
    pid = NA_integer_,
    container_id = "",
    objective = NA_real_,
    error = "",
    encrypted = encrypted,
    limits = list(
      max_cpu = limits$max_cpu,
      max_memory_mb = limits$max_memory_mb
    )
  )
  if (!is.null(payload$data_ref)) {
    meta$data_ref <- payload$data_ref
  }
  .ls_save_job_meta(meta, job_path)

  .ls_job_try_launch(username, job_id, limits)
}

#' @keywords internal
.ls_user_dispatch_lock_path <- function(username) {
  file.path(.ls_user_jobs_root(username), ".dispatch.lock")
}

#' @keywords internal
.ls_with_user_dispatch_lock <- function(username, expr) {
  root <- .ls_user_jobs_root(username)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  lock_path <- .ls_user_dispatch_lock_path(username)
  repeat {
    ok <- tryCatch({
      con <- file(lock_path, open = "wx")
      close(con)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      break
    }
    Sys.sleep(0.05)
  }
  on.exit(tryCatch(unlink(lock_path), error = function(e) NULL), add = TRUE)
  force(expr)
}

#' @keywords internal
.ls_job_try_launch <- function(username, job_id, limits) {
  .ls_with_user_dispatch_lock(username, {
    meta <- .ls_job_read_meta(username, job_id)
    if (is.null(meta) || !identical(meta$status, "queued")) {
      return(meta)
    }
    max_jobs <- .ls_effective_max_concurrent(limits)
    if (.ls_user_running_jobs(username) >= max_jobs) {
      return(meta)
    }
    .ls_proc_sweep()
    job_path <- .ls_job_path(username, job_id)
    .ls_clear_stale_job_artifacts(job_path)
    # For encrypted jobs, unwrap the DEK with the owner's cached key and pass it
    # to the worker in memory (never on disk). If the owner is not currently
    # authenticated we cannot launch yet - leave it queued for a later poll.
    dek_hex <- ""
    if (file.exists(file.path(job_path, "key.enc"))) {
      uk <- .ls_uk_get(username)
      if (is.null(uk)) {
        return(meta)
      }
      key_env <- .ls_read_rds_safe(file.path(job_path, "key.enc"))
      dek <- tryCatch(.ls_dek_unwrap(key_env, uk), error = function(e) NULL)
      if (is.null(dek)) {
        return(meta)
      }
      dek_hex <- .ls_bin2hex(dek)
    }
    launch <- .ls_launch_job(username, job_id, limits, dek_hex = dek_hex)
    meta$status <- "running"
    meta$started <- .ls_now()
    meta$pid <- launch$pid %||% NA_integer_
    meta$container_id <- launch$container_id %||% ""
    meta$launcher <- launch$mode
    .ls_save_job_meta(meta, job_path)
    if (!is.null(launch$process)) {
      # Keep a strong in-memory reference so the processx cleanup finalizer
      # cannot kill this worker while it is still running.
      .ls_proc_register(job_id, launch$process)
      saveRDS(launch$process, file.path(job_path, ".process.rds"))
    }
    meta
  })
}

#' @keywords internal
.ls_job_next_queued <- function(username) {
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(NULL)
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0L) {
    return(NULL)
  }
  rows <- lapply(ids, function(id) {
    meta <- .ls_job_read_meta(username, id)
    if (is.null(meta) || !identical(meta$status, "queued")) {
      return(NULL)
    }
    list(id = id, created = meta$created %||% "")
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(NULL)
  }
  ord <- order(vapply(rows, function(x) x$created, character(1L)))
  rows[[ord[[1L]]]]$id
}

#' @keywords internal
.ls_native_lock_path <- function() {
  file.path(ls_sandbox_root(), ".worker_native.lock")
}

#' @keywords internal
.ls_native_lock_stale <- function(lock_path = .ls_native_lock_path()) {
  if (!file.exists(lock_path)) {
    return(TRUE)
  }
  lines <- .ls_read_lines_safe(lock_path)
  if (length(lines) < 2L) {
    # Ambiguous read: treat as held so we don't start an overlapping worker.
    return(FALSE)
  }
  pid <- suppressWarnings(as.integer(lines[[2L]]))
  if (!is.na(pid) && pid > 0L && identical(.ls_pid_alive(pid), FALSE)) {
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

#' Non-blocking readiness check: a per-user slot is free, the server-wide cap is
#' not exceeded, and (only when native serialization is enabled) no native worker
#' still holds the lock. Never sleeps/blocks the (single-threaded) API.
#' @keywords internal
.ls_dispatch_ready <- function(username, max_jobs, cfg = NULL) {
  if (is.null(cfg)) {
    cfg <- ls_config()
  }
  if (.ls_user_running_jobs(username) >= max_jobs) {
    return(FALSE)
  }
  global_cap <- as.integer(cfg$max_global_running %||% 0L)
  if (!is.na(global_cap) && global_cap > 0L &&
      .ls_global_running_jobs() >= global_cap) {
    return(FALSE)
  }
  # The global native lock only gates dispatch when serialization is requested.
  if (isTRUE(cfg$worker_serial_native %||% FALSE)) {
    return(.ls_native_lock_stale())
  }
  TRUE
}

#' Launch queued jobs while capacity is free. This is NON-BLOCKING: if the
#' native lock is still held (previous worker finishing) or a slot is busy, it
#' returns immediately and relies on the next status/list poll to retry. A
#' blocking wait here would freeze the single-threaded plumber API and, if the
#' queue drained between polls, could stall dispatch indefinitely.
#' @keywords internal
.ls_job_dispatch_queue <- function(username, limits) {
  .ls_job_refresh_stale_running(username)
  max_jobs <- .ls_effective_max_concurrent(limits)
  launched_any <- FALSE
  while (.ls_dispatch_ready(username, max_jobs)) {
    nxt <- .ls_job_next_queued(username)
    if (is.null(nxt)) {
      break
    }
    launched <- .ls_job_try_launch(username, nxt, limits)
    if (is.null(launched) || identical(launched$status, "queued")) {
      break
    }
    launched_any <- TRUE
  }
  invisible(launched_any)
}

#' @keywords internal
.ls_job_status_label <- function(status) {
  if (identical(status, "queued")) {
    return("in queue")
  }
  as.character(status %||% "")
}

#' @keywords internal
.ls_job_read_meta <- function(username, job_id) {
  path <- file.path(.ls_job_path(username, job_id), "meta.rds")
  if (!file.exists(path)) {
    return(NULL)
  }
  .ls_read_rds_safe(path)
}

#' @keywords internal
.ls_job_start_grace_sec <- function() {
  5
}

#' @keywords internal
.ls_job_in_start_grace <- function(meta) {
  started <- meta$started %||% meta$created %||% ""
  if (!nzchar(started)) {
    return(FALSE)
  }
  t0 <- suppressWarnings(as.POSIXct(started, tz = ""))
  if (is.na(t0)) {
    return(FALSE)
  }
  difftime(Sys.time(), t0, units = "secs") < .ls_job_start_grace_sec()
}

#' Short-lived cache of the full set of running PIDs. Enumerating processes is
#' expensive on Windows (`tasklist` spawns a process and scans the whole system).
#' Reconcile/list operations can probe many PIDs per poll, so caching one
#' snapshot for a fraction of a second collapses N process spawns into 1 and is
#' the single biggest fix for the API pegging a core under bursty job load.
#' @keywords internal
.ls_pid_snapshot <- new.env(parent = emptyenv())

#' @keywords internal
.ls_running_pids <- function(ttl = getOption("LibeRties.pid_cache_ttl", 1)) {
  now <- Sys.time()
  cached <- .ls_pid_snapshot$pids
  ts <- .ls_pid_snapshot$ts
  if (!is.null(cached) && !is.null(ts) &&
      as.numeric(difftime(now, ts, units = "secs")) < ttl) {
    return(cached)
  }
  pids <- integer(0L)
  if (.Platform$OS.type == "windows") {
    out <- suppressWarnings(tryCatch(
      system2("tasklist", c("/FO", "CSV", "/NH"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    ))
    if (length(out) > 0L) {
      m <- regmatches(out, regexpr('"[^"]*","[0-9]+"', out))
      if (length(m) > 0L) {
        pid_str <- sub('.*","([0-9]+)".*', "\\1", m)
        pids <- suppressWarnings(as.integer(pid_str))
        pids <- pids[!is.na(pids)]
      }
    }
  }
  .ls_pid_snapshot$pids <- pids
  .ls_pid_snapshot$ts <- now
  pids
}

#' @keywords internal
.ls_pid_alive <- function(pid) {
  if (is.null(pid) || length(pid) != 1L || is.na(pid) || pid <= 0L) {
    return(NA)
  }
  if (.Platform$OS.type == "windows") {
    pids <- .ls_running_pids()
    if (length(pids) == 0L) {
      # Enumeration failed (not "no processes"): report unknown so callers fall
      # back to heartbeat/log evidence rather than declaring the worker dead.
      return(NA)
    }
    return(as.integer(pid) %in% pids)
  }
  ps_ok <- suppressWarnings(tryCatch({
    system2("ps", c("-p", as.character(pid)), stdout = FALSE, stderr = FALSE) == 0L
  }, error = function(e) NA))
  if (!is.na(ps_ok)) {
    return(ps_ok)
  }
  file.exists(file.path("/proc", as.character(pid)))
}

#' @keywords internal
.ls_container_running <- function(container_id) {
  if (is.null(container_id) || length(container_id) != 1L || !nzchar(container_id)) {
    return(NA)
  }
  out <- tryCatch(
    system2(
      "docker",
      c("inspect", "-f", "{{.State.Running}}", container_id),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character()
  )
  if (length(out) == 0L) {
    return(FALSE)
  }
  identical(tolower(trimws(out[[1L]])), "true")
}

#' @keywords internal
.ls_job_process_alive <- function(job_path) {
  # Only the live in-memory handle gives a reliable answer. A processx object
  # read back from .process.rds has lost its OS handle, so its is_alive() is
  # unreliable and can wrongly report a healthy worker as dead (which then
  # gets mislabelled "Worker exited without result."). When we don't hold the
  # live handle, return NA so the caller falls back to pid/heartbeat/log.
  job_id <- basename(job_path)
  if (exists(job_id, envir = .ls_proc_registry, inherits = FALSE)) {
    proc <- tryCatch(get(job_id, envir = .ls_proc_registry, inherits = FALSE),
                     error = function(e) NULL)
    if (!is.null(proc) && is.function(proc$is_alive)) {
      alive <- suppressWarnings(tryCatch(proc$is_alive(), error = function(e) NA))
      if (identical(alive, TRUE)) {
        return(TRUE)
      }
      if (identical(alive, FALSE)) {
        return(FALSE)
      }
    }
  }
  NA
}

#' @keywords internal
.ls_job_elapsed_sec <- function(meta) {
  started <- meta$started %||% meta$created %||% ""
  if (!nzchar(started)) {
    return(NA_real_)
  }
  t0 <- suppressWarnings(as.POSIXct(started, tz = ""))
  if (is.na(t0)) {
    return(NA_real_)
  }
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

#' @keywords internal
.ls_job_active_pid <- function(meta, job_path) {
  pid_path <- file.path(job_path, "worker.pid")
  if (file.exists(pid_path)) {
    pid <- suppressWarnings(as.integer(.ls_read_lines_safe(pid_path)[1L]))
    if (!is.na(pid) && pid > 0L) {
      return(pid)
    }
  }
  meta$pid
}

#' @keywords internal
.ls_job_estimation_phase <- function(job_path) {
  log_path <- file.path(job_path, "worker.log")
  if (!file.exists(log_path)) {
    return(FALSE)
  }
  lines <- .ls_read_lines_safe(log_path)
  txt <- paste(lines, collapse = "\n")
  grepl("Running estimation:|Running simulation:", txt) &&
    !grepl("Job completed\\.|Simulation completed:|Job failed:|Worker error:", txt)
}

#' @keywords internal
.ls_job_worker_likely_running <- function(meta, job_path) {
  log_hint <- .ls_job_log_status_hint(job_path)
  if (identical(log_hint, "success") || identical(log_hint, "error")) {
    return(FALSE)
  }
  if (.ls_job_worker_never_started(meta, job_path)) {
    return(FALSE)
  }
  proc_alive <- .ls_job_process_alive(job_path)
  if (identical(proc_alive, TRUE)) {
    return(TRUE)
  }
  if (identical(proc_alive, FALSE)) {
    return(FALSE)
  }
  pid <- .ls_job_active_pid(meta, job_path)
  alive <- .ls_pid_alive(pid)
  if (identical(alive, TRUE)) {
    return(TRUE)
  }
  if (identical(alive, FALSE)) {
    return(FALSE)
  }
  if (.ls_job_log_loading_phase(job_path, meta)) {
    elapsed <- .ls_job_elapsed_sec(meta)
    if (!is.finite(elapsed) || elapsed < .ls_job_loading_grace_sec()) {
      return(TRUE)
    }
  }
  cid <- meta$container_id %||% ""
  if (nzchar(cid)) {
    running <- .ls_container_running(cid)
    if (identical(running, TRUE)) {
      return(TRUE)
    }
    if (identical(running, FALSE)) {
      return(FALSE)
    }
  }
  elapsed <- .ls_job_elapsed_sec(meta)
  if (is.finite(elapsed) && elapsed < .ls_job_start_grace_sec()) {
    return(TRUE)
  }
  hb_path <- file.path(job_path, "worker.heartbeat")
  if (file.exists(hb_path)) {
    hb <- suppressWarnings(as.POSIXct(.ls_read_lines_safe(hb_path)[1L], tz = ""))
    if (!is.na(hb) && difftime(Sys.time(), hb, units = "secs") < 120) {
      return(TRUE)
    }
  }
  log_path <- file.path(job_path, "worker.log")
  if (file.exists(log_path)) {
    if (isTRUE(file.info(log_path)$mtime > Sys.time() - 120)) {
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
.ls_job_worker_log_hint <- function(job_path) {
  log_path <- file.path(job_path, "worker.log")
  parts <- character()
  if (file.exists(log_path)) {
    lines <- .ls_read_lines_safe(log_path)
    if (length(lines) > 0L) {
      parts <- c(parts, lines[max(1L, length(lines) - 4L):length(lines)])
    }
  }
  stderr_path <- file.path(job_path, "worker.stderr")
  if (file.exists(stderr_path)) {
    se <- .ls_read_lines_safe(stderr_path)
    if (length(se) > 0L) {
      parts <- c(parts, "--- stderr ---", se[max(1L, length(se) - 9L):length(se)])
    }
  }
  if (length(parts) == 0L) {
    return("")
  }
  paste(parts, collapse = "\n")
}

#' TRUE if a job produced a result, whether plaintext or encrypted.
#' @keywords internal
.ls_job_has_result <- function(job_path) {
  file.exists(file.path(job_path, "result.rds")) ||
    file.exists(file.path(job_path, "result.enc"))
}

#' Read a job result object, transparently decrypting result.enc when the
#' owner's key is available in memory. Returns NULL if unreadable/locked.
#' @keywords internal
.ls_read_result <- function(job_path, username = NULL) {
  enc <- file.path(job_path, "result.enc")
  if (file.exists(enc)) {
    uk <- if (!is.null(username)) .ls_uk_get(username) else NULL
    if (is.null(uk)) {
      return(NULL)
    }
    key_env <- .ls_read_rds_safe(file.path(job_path, "key.enc"))
    if (is.null(key_env)) {
      return(NULL)
    }
    dek <- tryCatch(.ls_dek_unwrap(key_env, uk), error = function(e) NULL)
    if (is.null(dek)) {
      return(NULL)
    }
    return(tryCatch(.ls_decrypt_from_file(dek, enc), error = function(e) NULL))
  }
  rds <- file.path(job_path, "result.rds")
  if (file.exists(rds)) {
    return(.ls_read_rds_safe(rds))
  }
  NULL
}

#' @keywords internal
.ls_job_reconcile <- function(meta, job_path) {
  if (is.null(meta)) {
    return(NULL)
  }
  error_path <- file.path(job_path, "error.txt")
  status <- meta$status %||% "queued"

  if (.ls_job_has_result(job_path) && .ls_job_result_trustworthy(meta, job_path)) {
    meta$status <- "success"
    meta$error <- ""
    if (identical(meta$job_type, "est")) {
      fit <- .ls_read_result(job_path, meta$user)
      if (!is.null(fit) && !is.null(fit$objective)) {
        meta$objective <- fit$objective
      }
    }
    if (meta$status %in% c("success", "error", "cancelled") &&
        (is.null(meta$finished) || !nzchar(meta$finished))) {
      log_path <- file.path(job_path, "worker.log")
      if (file.exists(log_path)) {
        meta$finished <- as.character(file.info(log_path)$mtime)
      } else {
        meta$finished <- .ls_now()
      }
    }
    return(meta)
  }

  running_like <- status %in% c("running", "queued")

  if (running_like) {
    if (identical(status, "running") && .ls_job_worker_never_started(meta, job_path)) {
      meta$status <- "error"
      meta$error <- "Worker never started."
      if (is.null(meta$finished) || !nzchar(meta$finished)) {
        meta$finished <- .ls_now()
      }
      if (!file.exists(error_path)) {
        writeLines(meta$error, error_path)
      }
      return(meta)
    }
    if (identical(status, "queued")) {
      return(meta)
    }
    if (identical(.ls_job_process_alive(job_path), TRUE)) {
      meta$status <- "running"
      meta$error <- ""
      if (file.exists(error_path)) {
        unlink(error_path)
      }
      return(meta)
    }
    if (.ls_job_in_start_grace(meta)) {
      meta$status <- "running"
      meta$error <- ""
      if (file.exists(error_path)) {
        unlink(error_path)
      }
      return(meta)
    }
    log_path <- file.path(job_path, "worker.log")
    if (file.exists(log_path)) {
      log_lines <- .ls_read_lines_safe(log_path)
      if (length(log_lines) > 0L) {
        log_tail <- log_lines[max(1L, length(log_lines) - 19L):length(log_lines)]
        log_txt <- paste(log_tail, collapse = "\n")
        if (grepl("Job failed:|Worker error:|Error in ", log_txt)) {
          meta$status <- "error"
          if (file.exists(error_path)) {
            meta$error <- paste(.ls_read_lines_safe(error_path), collapse = "\n")
          } else if (grepl("Job failed:", log_txt, fixed = TRUE)) {
            meta$error <- sub(".*Job failed:\\s*", "", log_tail[length(log_tail)])
          } else if (grepl("Worker error:", log_txt, fixed = TRUE)) {
            meta$error <- sub(".*Worker error:\\s*", "", log_tail[length(log_tail)])
          } else {
            meta$error <- log_tail[length(log_tail)]
          }
          if (meta$status %in% c("success", "error", "cancelled") &&
              (is.null(meta$finished) || !nzchar(meta$finished))) {
            meta$finished <- as.character(file.info(log_path)$mtime)
          }
          return(meta)
        }
        if (grepl("Job completed\\.|Simulation completed:", log_txt)) {
          if (.ls_job_has_result(job_path) && .ls_job_result_trustworthy(meta, job_path)) {
            meta$status <- "success"
            meta$error <- ""
            if (identical(meta$job_type, "est")) {
              fit <- .ls_read_result(job_path, meta$user)
              if (!is.null(fit) && !is.null(fit$objective)) {
                meta$objective <- fit$objective
              }
            }
            meta$finished <- as.character(file.info(log_path)$mtime)
            return(meta)
          }
          meta$status <- "error"
          meta$error <- "Worker log reports completion but result is missing."
          meta$finished <- as.character(file.info(log_path)$mtime)
          return(meta)
        }
      }
    }
    if (.ls_job_worker_likely_running(meta, job_path)) {
      meta$status <- "running"
      meta$error <- ""
      if (file.exists(error_path)) {
        unlink(error_path)
      }
      return(meta)
    }
    meta$status <- "error"
    if (is.null(meta$error) || !nzchar(meta$error)) {
      hint <- .ls_job_worker_log_hint(job_path)
      meta$error <- if (nzchar(hint)) {
        paste0("Worker exited without result.\n", hint)
      } else {
        "Worker exited without result."
      }
    }
    if (!file.exists(error_path)) {
      writeLines(meta$error, error_path)
    }
    return(meta)
  }

  if (file.exists(error_path) && !file.exists(result_path)) {
    meta$status <- "error"
    meta$error <- paste(.ls_read_lines_safe(error_path), collapse = "\n")
  }
  if (identical(meta$status, "error") && file.exists(result_path)) {
    meta$status <- "success"
    meta$error <- ""
    if (identical(meta$job_type, "est")) {
      fit <- .ls_read_rds_safe(result_path)
      if (!is.null(fit) && !is.null(fit$objective)) {
        meta$objective <- fit$objective
      }
    }
  }
  if (meta$status %in% c("success", "error", "cancelled") &&
      (is.null(meta$finished) || !nzchar(meta$finished))) {
    log_path <- file.path(job_path, "worker.log")
    if (file.exists(log_path)) {
      meta$finished <- as.character(file.info(log_path)$mtime)
    } else {
      meta$finished <- .ls_now()
    }
  }
  meta
}

#' @keywords internal
#' @param dispatch When TRUE (default) attempt a non-blocking queue dispatch
#'   after reconciling. Callers that already dispatch once for a whole batch
#'   (e.g. `.ls_job_list`) pass FALSE to avoid an O(N^2) dispatch/reconcile
#'   storm that pegs the single-threaded API when many jobs exist.
.ls_job_status <- function(username, job_id, dispatch = TRUE) {
  job_path <- .ls_job_path(username, job_id)
  meta <- .ls_job_read_meta(username, job_id)
  if (is.null(meta)) {
    return(NULL)
  }
  old <- meta$status
  meta <- .ls_job_reconcile(meta, job_path)
  if (meta$status %in% c("success", "error", "cancelled")) {
    .ls_proc_unregister(job_id)
  }
  if (!identical(old, meta$status)) {
    .ls_save_job_meta(meta, job_path)
  }
  # Attempt a (non-blocking) dispatch so polling a job keeps the queue moving,
  # even after all running jobs have drained. Suppressed for batch callers that
  # dispatch once themselves.
  if (isTRUE(dispatch)) {
    .ls_job_dispatch_queue(username, .ls_user_limits(username))
  }
  meta
}

#' @keywords internal
.ls_job_list_row <- function(st, id) {
  data.frame(
    id = st$id %||% id,
    label = st$label %||% id,
    job_type = st$job_type %||% "est",
    status = st$status,
    method = st$method %||% "",
    created = st$created %||% "",
    started = st$started %||% "",
    finished = st$finished %||% "",
    objective = st$objective %||% NA_real_,
    error = as.character(st$error %||% ""),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.ls_job_list <- function(username, reconcile = c("active", "all", "none")) {
  reconcile <- match.arg(reconcile)
  if (!identical(reconcile, "none")) {
    .ls_job_dispatch_queue(username, .ls_user_limits(username))
  }
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(data.frame())
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0L) {
    return(data.frame())
  }
  rows <- lapply(ids, function(id) {
    meta <- .ls_job_read_meta(username, id)
    if (is.null(meta)) {
      return(NULL)
    }
    terminal <- meta$status %in% c("success", "error", "cancelled")
    job_path <- .ls_job_path(username, id)
    st <- if (identical(reconcile, "none") || terminal) {
      # Terminal jobs keep their persisted final status. We deliberately do NOT
      # re-run .ls_job_status here: it re-reads result.rds and re-dispatches
      # (scanning every job) on each poll, which turned a job list into an
      # O(N^2) CPU storm that pegged the single-threaded API. The only recovery
      # case is an "error" job with no result that still looks alive (a rare
      # transient mislabel); reconcile that one without dispatching.
      if (terminal && !identical(reconcile, "none") &&
          identical(meta$status, "error") &&
          !.ls_job_has_result(job_path) &&
          .ls_job_worker_likely_running(meta, job_path)) {
        .ls_job_status(username, id, dispatch = FALSE)
      } else {
        meta
      }
    } else {
      .ls_job_status(username, id, dispatch = FALSE)
    }
    if (is.null(st)) {
      return(NULL)
    }
    if (identical(st$status, "success") && identical(st$job_type, "est") &&
        (is.null(st$objective) || !is.finite(st$objective))) {
      fit <- .ls_read_result(job_path, username)
      if (!is.null(fit) && !is.null(fit$objective)) {
        st$objective <- fit$objective
      }
    }
    .ls_job_list_row(st, id)
  })
  # Fill any slot freed by a completion detected during this reconcile pass.
  # Runs once per list (not once per job); cheap now that PID probes are cached.
  if (!identical(reconcile, "none")) {
    .ls_job_dispatch_queue(username, .ls_user_limits(username))
  }
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(data.frame())
  }
  df <- do.call(rbind, rows)
  df[order(df$created, decreasing = TRUE), , drop = FALSE]
}

#' @keywords internal
.ls_job_log <- function(username, job_id, tail = 100L) {
  log_path <- file.path(.ls_job_path(username, job_id), "worker.log")
  if (!file.exists(log_path)) {
    return("")
  }
  lines <- .ls_read_lines_safe(log_path)
  if (length(lines) > tail) {
    lines <- lines[(length(lines) - tail + 1L):length(lines)]
  }
  paste(lines, collapse = "\n")
}

#' @keywords internal
.ls_job_result_b64 <- function(username, job_id) {
  job_path <- .ls_job_path(username, job_id)
  enc <- file.path(job_path, "result.enc")
  if (file.exists(enc)) {
    obj <- .ls_read_result(job_path, username)
    if (is.null(obj)) {
      stop(
        "Result is encrypted and could not be decrypted. Ensure you are ",
        "authenticated with the owning user's current API token.",
        call. = FALSE
      )
    }
    # Emit RDS-format bytes (matching the plaintext path) so the client's
    # readRDS-based decoder works unchanged. Travels to the client over TLS.
    con <- rawConnection(raw(0L), "w")
    on.exit(close(con), add = TRUE)
    saveRDS(obj, con)
    return(jsonlite::base64_enc(rawConnectionValue(con)))
  }
  result_path <- file.path(job_path, "result.rds")
  if (!file.exists(result_path)) {
    stop("Result not available.", call. = FALSE)
  }
  jsonlite::base64_enc(readBin(result_path, raw(), file.info(result_path)$size))
}

#' @keywords internal
.ls_job_cleanup <- function(username,
                            status = c("success", "error", "cancelled")) {
  root <- .ls_user_jobs_root(username)
  if (!dir.exists(root)) {
    return(0L)
  }
  ids <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  n <- 0L
  for (id in ids) {
    meta <- .ls_job_read_meta(username, id)
    if (is.null(meta) || !meta$status %in% status) {
      next
    }
    unlink(.ls_job_path(username, id), recursive = TRUE)
    n <- n + 1L
  }
  n
}

#' @keywords internal
.ls_job_cleanup_all <- function(status = c("success", "error", "cancelled")) {
  root <- file.path(ls_sandbox_root(), "sandboxes")
  if (!dir.exists(root)) {
    return(0L)
  }
  users <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  users <- users[nzchar(users)]
  n <- 0L
  for (u in users) {
    n <- n + .ls_job_cleanup(u, status = status)
  }
  n
}

#' @keywords internal
.ls_job_cancel <- function(username, job_id) {
  meta <- .ls_job_read_meta(username, job_id)
  if (is.null(meta)) {
    stop("Unknown job.", call. = FALSE)
  }
  if (meta$status %in% c("success", "error", "cancelled")) {
    return(meta)
  }
  job_path <- .ls_job_path(username, job_id)
  if (identical(meta$status, "running")) {
    # Prefer the live in-memory process handle; a handle deserialized from
    # .process.rds in another session cannot actually signal the child.
    proc <- if (exists(job_id, envir = .ls_proc_registry, inherits = FALSE)) {
      tryCatch(get(job_id, envir = .ls_proc_registry, inherits = FALSE),
               error = function(e) NULL)
    } else {
      NULL
    }
    if (!is.null(proc)) {
      tryCatch(proc$kill(), error = function(e) NULL)
    } else {
      pid <- suppressWarnings(as.integer(meta$pid))
      if (!is.na(pid) && pid > 0L) {
        tryCatch(tools::pskill(pid), error = function(e) NULL)
      }
    }
    cid <- meta$container_id %||% ""
    if (nzchar(cid)) {
      system2("docker", c("stop", cid), stdout = FALSE, stderr = FALSE)
    }
  }
  .ls_proc_unregister(job_id)
  meta$status <- "cancelled"
  meta$finished <- .ls_now()
  .ls_save_job_meta(meta, job_path)
  .ls_job_dispatch_queue(username, .ls_user_limits(username))
  meta
}

#' @keywords internal
.ls_job_list_all <- function(reconcile = c("active", "all", "none"),
                             dispatch = TRUE) {
  reconcile <- match.arg(reconcile)
  root <- file.path(ls_sandbox_root(), "sandboxes")
  if (!dir.exists(root)) {
    return(data.frame())
  }
  users <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  users <- users[nzchar(users)]
  if (isTRUE(dispatch)) {
    for (u in users) {
      if (.ls_user_active_jobs(u) > 0L) {
        .ls_job_dispatch_queue(u, .ls_user_limits(u))
      }
    }
  }
  parts <- lapply(users, function(u) {
    df <- .ls_job_list(u, reconcile = reconcile)
    if (nrow(df) == 0L) {
      return(NULL)
    }
    df$user <- u
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1L))]
  if (length(parts) == 0L) {
    return(data.frame())
  }
  do.call(rbind, parts)
}
