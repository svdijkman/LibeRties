#' Persistent cross-platform LibeR job queue
#'
#' Each user receives a separate filesystem namespace. Live jobs run in fresh
#' `callr` R processes and reconstruct C++ pointers from serialized models.
#'
#' @export
LibeRQueue <- R6::R6Class(
  "LibeRQueue",
  public = list(
    #' @field root Normalized persistent queue storage directory.
    root = NULL,
    #' @field user Safe tenant namespace used by default for queue operations.
    user = NULL,
    #' @field max_workers Maximum simultaneous background workers.
    max_workers = NULL,
    #' @field limits Effective resource and storage limits.
    limits = NULL,
    #' @field isolation Description of the worker isolation strategy.
    isolation = NULL,
    #' @field processes Internal environment of live `callr` worker handles.
    processes = NULL,

    #' @description
    #' Create or reopen a persistent local queue.
    #' @param root Persistent queue storage directory.
    #' @param user Default isolated tenant namespace.
    #' @param max_workers Maximum simultaneous background workers.
    #' @param limits Named overrides for runtime, CPU, memory, payload, result,
    #'   concurrency, queue, and storage limits.
    #' @return A new `LibeRQueue` object.
    initialize = function(root = .ls_default_root(), user = "local",
                          max_workers = 1L, limits = list()) {
      self$root <- .ls_ensure_dir(root)
      self$user <- .ls_safe_component(user, "user id")
      self$max_workers <- as.integer(max_workers)
      self$limits <- .ls_limits(limits)
      self$max_workers <- min(self$max_workers, self$limits$max_concurrent_jobs)
      self$isolation <- "restricted-subprocess"
      if (length(self$max_workers) != 1L || is.na(self$max_workers) || self$max_workers < 1L) {
        .ls_stop("`max_workers` must be a positive integer.")
      }
      self$processes <- new.env(parent = emptyenv())
      .ls_ensure_dir(file.path(self$root, "users", self$user, "jobs"))
    },

    #' @description
    #' Persist a serializable job and optionally start available workers.
    #' @param job A job created by [ls_job()].
    #' @param user Tenant namespace receiving the job.
    #' @param start Start available queued work immediately.
    #' @return The durable job identifier, invisibly.
    submit = function(job, user = self$user, start = TRUE) {
      if (!inherits(job, "liber_job")) .ls_stop("`job` must be created by ls_job().")
      user <- .ls_safe_component(user, "user id")
      payload_bytes <- length(serialize(job, NULL, version = 3))
      if (payload_bytes > self$limits$max_payload_mb * 1024^2) {
        .ls_stop("Payload exceeds this queue's max_payload_mb limit.")
      }
      jobs <- self$list(user)
      if (sum(jobs$status == "queued") >= self$limits$max_queued_jobs) {
        .ls_stop("Queued-job limit reached for user ", user, ".")
      }
      if (.ls_storage_bytes(self$root, user) + payload_bytes >
          self$limits$max_storage_mb * 1024^2) {
        .ls_stop("Storage quota reached for user ", user, ".")
      }
      id <- .ls_new_id()
      job_dir <- .ls_job_dir(self$root, user, id)
      if (!dir.create(job_dir, recursive = FALSE, showWarnings = FALSE)) {
        .ls_stop("Unable to create job sandbox: ", job_dir)
      }
      payload <- .ls_payload_path(job_dir)
      .ls_atomic_save_rds(job, payload)
      metadata <- list(
        schema = "liber.queue.metadata", version = 2L, id = id, user = user,
        type = job$type, label = job$label, status = "queued",
        submitted = .ls_now(), started = "", finished = "", updated = .ls_now(),
        pid = NA_integer_, pid_started = NA_real_, error = "", payload_md5 = .ls_md5(payload),
        payload_sha256 = .ls_sha256(payload), result_md5 = "",
        result_sha256 = "", result_bytes = 0,
        limits = self$limits, isolation = self$isolation,
        peak_memory_mb = 0, cpu_seconds = 0, elapsed_seconds = 0,
        termination_reason = ""
      )
      .ls_write_meta(job_dir, metadata)
      if (isTRUE(start)) self$poll(start = TRUE)
      invisible(id)
    },

    #' @description
    #' Refresh worker state, enforce limits, recover jobs, and start queued work.
    #' @param start Whether to start queued work when capacity is available.
    #' @return The current job table, invisibly.
    poll = function(start = TRUE) {
      private$enforce_limits()
      private$reap()
      private$recover_untracked()
      if (isTRUE(start)) private$start_available()
      invisible(self$list())
    },

    #' @description
    #' Read durable metadata for one job.
    #' @param id Durable job identifier.
    #' @param user Tenant namespace owning the job.
    #' @return A named metadata list.
    status = function(id, user = self$user) {
      job_dir <- .ls_job_dir(self$root, user, id)
      if (!file.exists(.ls_meta_path(job_dir))) .ls_stop("Unknown job id.")
      .ls_read_meta(job_dir)
    },

    #' @description
    #' List durable jobs in a tenant namespace.
    #' @param user Tenant namespace to list.
    #' @return A data frame ordered from newest to oldest submission.
    list = function(user = self$user) {
      user <- .ls_safe_component(user, "user id")
      root <- file.path(self$root, "users", user, "jobs")
      if (!dir.exists(root)) return(.ls_empty_jobs())
      directories <- list.dirs(root, full.names = TRUE, recursive = FALSE)
      if (!length(directories)) return(.ls_empty_jobs())
      records <- lapply(directories, function(path) {
        tryCatch(.ls_read_meta(path), error = function(e) NULL)
      })
      records <- Filter(Negate(is.null), records)
      if (!length(records)) return(.ls_empty_jobs())
      result <- do.call(rbind, lapply(records, function(x) data.frame(
        id = x$id, user = x$user, type = x$type, label = x$label,
        status = x$status, submitted = x$submitted, started = x$started,
        finished = x$finished, stringsAsFactors = FALSE
      )))
      result[order(result$submitted, decreasing = TRUE), , drop = FALSE]
    },

    #' @description
    #' Read and verify a completed job result.
    #' @param id Durable job identifier.
    #' @param user Tenant namespace owning the job.
    #' @return The deserialized job result.
    result = function(id, user = self$user) {
      job_dir <- .ls_job_dir(self$root, user, id)
      metadata <- .ls_read_meta(job_dir)
      if (!identical(metadata$status, "completed")) {
        .ls_stop("Job ", id, " is ", metadata$status, "; no completed result is available.")
      }
      path <- .ls_result_path(job_dir)
      if (!.ls_digest_matches(path, metadata, "result")) {
        .ls_stop("Result checksum mismatch for job ", id, ".")
      }
      .ls_read_rds(path)
    },

    #' @description
    #' Read a worker log stream.
    #' @param id Durable job identifier.
    #' @param user Tenant namespace owning the job.
    #' @param stream Standard-output or standard-error stream.
    #' @return A character vector containing log lines.
    logs = function(id, user = self$user, stream = c("stdout", "stderr")) {
      stream <- match.arg(stream)
      path <- file.path(.ls_job_dir(self$root, user, id), paste0(stream, ".log"))
      if (!file.exists(path)) return(character())
      readLines(path, warn = FALSE, encoding = "UTF-8")
    },

    #' @description
    #' Cancel a queued or running job.
    #' @param id Durable job identifier.
    #' @param user Tenant namespace owning the job.
    #' @return `TRUE` when cancellation changed the job state and `FALSE` when
    #'   the job was already terminal, invisibly.
    cancel = function(id, user = self$user) {
      job_dir <- .ls_job_dir(self$root, user, id)
      metadata <- .ls_read_meta(job_dir)
      if (.ls_terminal(metadata$status)) return(invisible(FALSE))
      key <- paste(user, id, sep = "::")
      if (exists(key, envir = self$processes, inherits = FALSE)) {
        process <- get(key, envir = self$processes, inherits = FALSE)
        if (isTRUE(process$is_alive())) {
          .ls_kill_process_tree(process$get_pid())
          try(process$kill(), silent = TRUE)
        }
      } else {
        pid <- suppressWarnings(as.integer(metadata$pid %||% NA_integer_))
        if (!is.na(pid) && pid > 0L) .ls_kill_process_tree(pid)
      }
      .ls_update_meta(job_dir, list(
        status = "cancelled", finished = .ls_now(), error = "Cancelled by user."
      ), allowed_status = c("queued", "running"))
      invisible(TRUE)
    },

    #' @description
    #' Poll until a job reaches a terminal state.
    #' @param id Durable job identifier.
    #' @param user Tenant namespace owning the job.
    #' @param timeout Maximum elapsed seconds; `Inf` waits indefinitely.
    #' @param poll_interval Delay between polls in seconds.
    #' @return Final job metadata.
    wait = function(id, user = self$user, timeout = Inf, poll_interval = 0.1) {
      started <- proc.time()[["elapsed"]]
      repeat {
        self$poll(start = TRUE)
        metadata <- self$status(id, user)
        if (.ls_terminal(metadata$status)) return(metadata)
        if (is.finite(timeout) && proc.time()[["elapsed"]] - started >= timeout) {
          .ls_stop("Timed out waiting for job ", id, ".")
        }
        Sys.sleep(max(0.01, as.numeric(poll_interval)))
      }
    },

    #' @description
    #' Print queue location, capacity, limits, and job count.
    #' @param ... Unused.
    #' @return The queue, invisibly.
    print = function(...) {
      jobs <- self$list()
      cat("LibeR local queue\n")
      cat("  root:", self$root, "\n")
      cat("  user:", self$user, " workers:", self$max_workers,
          " jobs:", nrow(jobs), "\n")
      cat("  isolation:", self$isolation, " memory:", self$limits$max_memory_mb,
          "MB runtime:", self$limits$max_runtime_seconds, "s\n")
      invisible(self)
    }
  ),
  private = list(
    enforce_limits = function() {
      keys <- ls(self$processes, all.names = TRUE)
      for (key in keys) {
        process <- get(key, envir = self$processes, inherits = FALSE)
        if (!isTRUE(process$is_alive())) next
        parts <- strsplit(key, "::", fixed = TRUE)[[1L]]
        job_dir <- .ls_job_dir(self$root, parts[[1L]], parts[[2L]])
        metadata <- .ls_read_meta(job_dir)
        if (.ls_terminal(metadata$status)) next
        limits <- .ls_limits(metadata$limits %||% self$limits)
        started <- suppressWarnings(as.POSIXct(
          metadata$started, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
        ))
        elapsed <- if (is.na(started)) 0 else as.numeric(difftime(Sys.time(), started, units = "secs"))
        usage <- tryCatch(.ls_resource_usage(process$get_pid()), error = function(e) {
          list(memory_mb = 0, cpu_seconds = 0)
        })
        peak <- max(as.numeric(metadata$peak_memory_mb %||% 0), usage$memory_mb)
        reason <- ""
        if (elapsed > limits$max_runtime_seconds) {
          reason <- paste0("wall-time limit exceeded (", limits$max_runtime_seconds, " seconds)")
        } else if (usage$cpu_seconds > limits$max_cpu_seconds) {
          reason <- paste0("CPU-time limit exceeded (", limits$max_cpu_seconds, " seconds)")
        } else if (usage$memory_mb > limits$max_memory_mb) {
          reason <- paste0("memory limit exceeded (", limits$max_memory_mb, " MB)")
        }
        if (nzchar(reason)) {
          .ls_kill_process_tree(process$get_pid())
          try(process$kill(), silent = TRUE)
          .ls_update_meta(job_dir, list(
            status = "failed", finished = .ls_now(),
            error = paste("Resource limit exceeded:", reason),
            termination_reason = reason, peak_memory_mb = peak,
            cpu_seconds = usage$cpu_seconds, elapsed_seconds = elapsed
          ), allowed_status = c("queued", "running"))
        } else {
          .ls_update_meta(job_dir, list(
            peak_memory_mb = peak, cpu_seconds = usage$cpu_seconds,
            elapsed_seconds = elapsed
          ), allowed_status = c("queued", "running"))
        }
      }
      invisible(NULL)
    },

    recover_untracked = function() {
      jobs <- self$list()
      jobs <- jobs[jobs$status == "running", , drop = FALSE]
      if (!nrow(jobs)) return(invisible(NULL))
      for (i in seq_len(nrow(jobs))) {
        key <- paste(jobs$user[[i]], jobs$id[[i]], sep = "::")
        if (exists(key, envir = self$processes, inherits = FALSE)) next
        job_dir <- .ls_job_dir(self$root, jobs$user[[i]], jobs$id[[i]])
        metadata <- .ls_read_meta(job_dir)
        pid <- suppressWarnings(as.integer(metadata$pid %||% NA_integer_))
        alive <- !is.na(pid) && pid > 0L && isTRUE(tryCatch(
          ps::ps_pid_exists(pid), error = function(e) FALSE
        ))
        expected_start <- suppressWarnings(as.numeric(metadata$pid_started %||% NA_real_))
        if (alive && is.finite(expected_start)) {
          actual_start <- tryCatch(ps::ps_create_time(ps::ps_handle(pid)), error = function(e) NA_real_)
          alive <- is.finite(actual_start) && abs(actual_start - expected_start) < 1
        }
        if (!alive) {
          .ls_update_meta(job_dir, list(
            status = "failed", finished = .ls_now(),
            error = "Worker process was not alive during durable-queue recovery."
          ), allowed_status = c("queued", "running"))
          next
        }
        limits <- .ls_limits(metadata$limits %||% self$limits)
        started <- suppressWarnings(as.POSIXct(
          metadata$started, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
        ))
        elapsed <- if (is.na(started)) 0 else {
          as.numeric(difftime(Sys.time(), started, units = "secs"))
        }
        usage <- tryCatch(.ls_resource_usage(pid), error = function(e) {
          list(memory_mb = 0, cpu_seconds = 0)
        })
        peak <- max(as.numeric(metadata$peak_memory_mb %||% 0), usage$memory_mb)
        reason <- ""
        if (elapsed > limits$max_runtime_seconds) {
          reason <- paste0("wall-time limit exceeded (", limits$max_runtime_seconds, " seconds)")
        } else if (usage$cpu_seconds > limits$max_cpu_seconds) {
          reason <- paste0("CPU-time limit exceeded (", limits$max_cpu_seconds, " seconds)")
        } else if (usage$memory_mb > limits$max_memory_mb) {
          reason <- paste0("memory limit exceeded (", limits$max_memory_mb, " MB)")
        }
        if (nzchar(reason)) {
          .ls_kill_process_tree(pid)
          .ls_update_meta(job_dir, list(
            status = "failed", finished = .ls_now(),
            error = paste("Resource limit exceeded after queue recovery:", reason),
            termination_reason = reason, peak_memory_mb = peak,
            cpu_seconds = usage$cpu_seconds, elapsed_seconds = elapsed
          ), allowed_status = "running")
        } else {
          .ls_update_meta(job_dir, list(
            peak_memory_mb = peak, cpu_seconds = usage$cpu_seconds,
            elapsed_seconds = elapsed
          ), allowed_status = "running")
        }
      }
      invisible(NULL)
    },

    reap = function() {
      keys <- ls(self$processes, all.names = TRUE)
      for (key in keys) {
        process <- get(key, envir = self$processes, inherits = FALSE)
        if (isTRUE(process$is_alive())) next
        parts <- strsplit(key, "::", fixed = TRUE)[[1L]]
        job_dir <- .ls_job_dir(self$root, parts[[1L]], parts[[2L]])
        metadata <- .ls_read_meta(job_dir)
        if (!.ls_terminal(metadata$status)) {
          code <- tryCatch(process$get_exit_status(), error = function(e) NA_integer_)
          .ls_update_meta(job_dir, list(
            status = "failed", finished = .ls_now(),
            error = paste0("Worker exited before publishing a result (exit ", code, ").")
          ), allowed_status = c("queued", "running"))
        }
        rm(list = key, envir = self$processes)
      }
    },

    start_available = function() {
      running <- self$list()
      metadata_running <- sum(running$status == "running")
      tracked_running <- sum(vapply(
        as.list(self$processes), function(p) isTRUE(p$is_alive()), logical(1)
      ))
      n_running <- max(metadata_running, tracked_running)
      slots <- self$max_workers - min(self$max_workers, n_running)
      if (slots <= 0L) return(invisible(NULL))
      queued <- running[running$status == "queued", , drop = FALSE]
      if (!nrow(queued)) return(invisible(NULL))
      queued <- queued[order(queued$submitted), , drop = FALSE]
      for (i in seq_len(min(slots, nrow(queued)))) {
        private$start_one(queued$user[[i]], queued$id[[i]])
      }
      invisible(NULL)
    },

    start_one = function(user, id) {
      job_dir <- .ls_job_dir(self$root, user, id)
      claim <- file.path(job_dir, ".claimed")
      if (!dir.create(claim, showWarnings = FALSE)) return(invisible(FALSE))
      metadata <- .ls_read_meta(job_dir)
      if (!identical(metadata$status, "queued")) return(invisible(FALSE))
      key <- paste(user, id, sep = "::")
      process <- tryCatch(
        r_bg(
          function(job_dir, library_paths) {
            .libPaths(unique(c(library_paths, .libPaths())))
            library(LibeRties)
            LibeRties:::.ls_run_job(job_dir)
          },
          args = list(job_dir = job_dir, library_paths = .libPaths()),
          libpath = .libPaths(), supervise = TRUE,
          wd = job_dir, env = .ls_worker_env(job_dir),
          stdout = file.path(job_dir, "stdout.log"),
          stderr = file.path(job_dir, "stderr.log")
        ),
        error = identity
      )
      if (inherits(process, "error")) {
        unlink(claim, recursive = TRUE, force = TRUE)
        .ls_update_meta(job_dir, list(
          status = "failed", finished = .ls_now(), error = conditionMessage(process)
        ), allowed_status = "queued")
        return(invisible(FALSE))
      }
      assign(key, process, envir = self$processes)
      invisible(TRUE)
    }
  )
)

#' Create a local LibeR queue
#'
#' @param root Persistent queue root.
#' @param user Isolated user namespace.
#' @param max_workers Maximum simultaneous worker subprocesses for this queue.
#' @param limits Named resource limits, including wall time, CPU time, memory,
#'   payload, result, and storage quotas.
#' @return A persistent `LibeRQueue` object.
#' @examples
#' queue <- ls_local_queue(tempfile("liberties-queue-"), max_workers = 1L)
#' queue$list()
#' @export
ls_local_queue <- function(root = .ls_default_root(), user = "local", max_workers = 1L,
                           limits = list()) {
  LibeRQueue$new(root = root, user = user, max_workers = max_workers, limits = limits)
}
