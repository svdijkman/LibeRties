#' Resolve a sibling package source tree (e.g. LibeRation next to LibeRties)
#' @keywords internal
.ls_sibling_pkg_root <- function(pkg_name) {
  if (!"LibeRties" %in% loadedNamespaces()) {
    return("")
  }
  ip <- system.file("", package = "LibeRties")
  if (!nzchar(ip)) {
    return("")
  }
  ip <- normalizePath(ip, winslash = "/", mustWork = FALSE)
  pkg_root <- if (file.exists(file.path(ip, "DESCRIPTION"))) {
    ip
  } else {
    normalizePath(file.path(ip, ".."), winslash = "/", mustWork = FALSE)
  }
  parent <- normalizePath(file.path(pkg_root, ".."), winslash = "/", mustWork = FALSE)
  cand <- file.path(parent, pkg_name)
  if (file.exists(file.path(cand, "DESCRIPTION"))) {
    return(normalizePath(cand, winslash = "/", mustWork = FALSE))
  }
  ""
}

#' Detect whether a path is a development package source tree
#' @keywords internal
.ls_is_dev_pkg_root <- function(root) {
  if (is.null(root) || !nzchar(root) || !dir.exists(root)) {
    return(FALSE)
  }
  if (file.exists(file.path(root, "Meta", "package.rds"))) {
    return(FALSE)
  }
  file.exists(file.path(root, "DESCRIPTION")) &&
    dir.exists(file.path(root, "R"))
}

#' Worker package environment saved in each job directory
#' @keywords internal
.ls_job_worker_env <- function() {
  cfg <- ls_config()
  use_installed <- cfg$worker_use_installed
  if (is.null(use_installed)) {
    use_installed <- FALSE
  }
  if (isTRUE(use_installed) &&
      requireNamespace("LibeRation", quietly = TRUE)) {
    return(list(mode = "installed", nm_root = "", ad_root = ""))
  }
  if ("LibeRation" %in% loadedNamespaces()) {
    fn <- get0(".nm_job_dev_env", envir = asNamespace("LibeRation"), inherits = FALSE)
    if (is.function(fn)) {
      env <- fn()
      if (nzchar(env$nm_root %||% "") && .ls_is_dev_pkg_root(env$nm_root)) {
        env$mode <- "dev"
        sib_ad <- .ls_sibling_pkg_root("LibeRtAD")
        if (nzchar(sib_ad)) {
          env$ad_root <- sib_ad
        }
      }
      return(env)
    }
  }

  cfg <- ls_config()
  nm_root <- as.character(cfg$liberation_root %||% "")
  ad_root <- as.character(cfg$libertad_root %||% "")
  if (!nzchar(nm_root)) {
    nm_root <- .ls_env("LIBERATION_ROOT", "LIBERATION_ROOT")
  }
  if (!nzchar(ad_root)) {
    ad_root <- .ls_env("LIBERTAD_ROOT", "LIBERTAD_ROOT")
  }
  if (!nzchar(nm_root)) {
    nm_root <- .ls_sibling_pkg_root("LibeRation")
  }
  if (!nzchar(ad_root)) {
    ad_root <- .ls_sibling_pkg_root("LibeRtAD")
  }
  if (!nzchar(ad_root) && nzchar(nm_root)) {
    for (cand in c(
      file.path(nm_root, "..", "LibeRtAD"),
      file.path(dirname(nm_root), "LibeRtAD")
    )) {
      if (dir.exists(cand) && file.exists(file.path(cand, "DESCRIPTION"))) {
        ad_root <- normalizePath(cand, winslash = "/", mustWork = FALSE)
        break
      }
    }
  }
  if (nzchar(nm_root) && .ls_is_dev_pkg_root(nm_root)) {
    sib_ad <- .ls_sibling_pkg_root("LibeRtAD")
    if (nzchar(sib_ad)) {
      ad_root <- sib_ad
    }
  }

  if (nzchar(nm_root) && .ls_is_dev_pkg_root(nm_root)) {
    return(list(mode = "dev", nm_root = nm_root, ad_root = ad_root))
  }
  if (requireNamespace("LibeRation", quietly = TRUE)) {
    return(list(mode = "installed", nm_root = "", ad_root = ""))
  }
  if (nzchar(nm_root)) {
    return(list(mode = "dev", nm_root = nm_root, ad_root = ad_root))
  }
  list(mode = "installed", nm_root = "", ad_root = "")
}

#' Record a worker failure in meta.rds, error.txt, and worker.log
#' @keywords internal
.ls_worker_record_error <- function(job_path, msg) {
  msg <- as.character(msg)[1L]
  if (is.na(msg) || !nzchar(msg)) {
    msg <- "Unknown worker error."
  }
  log_path <- file.path(job_path, "worker.log")
  cat("Worker error:", msg, "\n", file = log_path, append = TRUE)
  err_path <- file.path(job_path, "error.txt")
  if (!file.exists(err_path)) {
    writeLines(msg, err_path)
  }
  meta_path <- file.path(job_path, "meta.rds")
  if (file.exists(meta_path)) {
    meta <- .ls_read_rds_safe(meta_path)
    if (!is.null(meta)) {
      meta$status <- "error"
      meta$error <- msg
      if (is.null(meta$finished) || !nzchar(meta$finished)) {
        meta$finished <- .ls_now()
      }
      .ls_save_rds_safe(meta, meta_path)
    }
  }
  invisible(msg)
}

#' Load LibeRation/LibeRtAD for a job worker process
#' @keywords internal
.ls_worker_load_pkg_root <- function(root, pkg, log_path = NULL) {
  log <- function(...) {
    if (!is.null(log_path)) {
      cat(..., file = log_path, append = TRUE)
    }
  }
  if (is.null(root) || !nzchar(root)) {
    return(invisible(FALSE))
  }
  if (.ls_is_dev_pkg_root(root)) {
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
  stop("Cannot load ", pkg, " (missing at ", root, ").", call. = FALSE)
}

#' @keywords internal
.ls_worker_load_packages <- function(dev_env, log_path = NULL) {
  log <- function(...) {
    if (!is.null(log_path)) {
      cat(..., file = log_path, append = TRUE)
    }
  }
  if (identical(dev_env$mode, "dev") && nzchar(dev_env$nm_root %||% "")) {
    .ls_worker_load_pkg_root(dev_env$ad_root %||% "", "LibeRtAD", log_path)
    .ls_worker_load_pkg_root(dev_env$nm_root, "LibeRation", log_path)
    return(invisible(TRUE))
  }
  if (!requireNamespace("LibeRation", quietly = TRUE)) {
    stop(
      "Package LibeRation is not installed and no development source path is configured. ",
      "Set LIBERATION_ROOT / liberation_root in config, or install LibeRation.",
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
  invisible(TRUE)
}

#' Run a job worker in a child R process
#' @keywords internal
.ls_run_job_worker <- function(job_path) {
  bootstrap <- system.file("worker", "bootstrap.R", package = "LibeRties")
  if (!nzchar(bootstrap) || !file.exists(bootstrap)) {
    stop("Worker bootstrap script not found.", call. = FALSE)
  }
  source(bootstrap, local = TRUE)
  run_job_worker(job_path)
}

#' Extract a useful error snippet from worker.log
#' @keywords internal
.ls_worker_log_snippet <- function(log_path, tail = 40L) {
  if (!file.exists(log_path)) {
    return("")
  }
  lines <- readLines(log_path, warn = FALSE)
  if (length(lines) == 0L) {
    return("")
  }
  if (length(lines) > tail) {
    lines <- lines[(length(lines) - tail + 1L):length(lines)]
  }
  err_idx <- grep(
    "^(Worker error:|Error in |Job failed:|Loading dev packages|Loading installed|Package .+ is not installed)",
    lines
  )
  if (length(err_idx) > 0L) {
    return(paste(lines[err_idx[1L]:length(lines)], collapse = "\n"))
  }
  paste(lines, collapse = "\n")
}
