#' Default per-user resource limits
#' @keywords internal
.ls_default_limits <- function() {
  list(
    max_concurrent_jobs = 1L,
    max_disk_mb = 5120L,
    max_cpu = 4L,
    max_memory_mb = 8192L
  )
}

#' Default scheduler configuration
#' @keywords internal
.ls_default_config <- function() {
  list(
    sandbox_root = .ls_default_sandbox_root(),
    # Bind the API to localhost by default: production traffic should arrive via
    # a TLS-terminating reverse proxy on the same host (see inst/deploy/). Set to
    # "0.0.0.0" only for trusted networks without TLS (a startup warning is shown).
    api_host = "127.0.0.1",
    api_port = 8080L,
    admin_host = "127.0.0.1",
    admin_port = 8081L,
    # CORS: only matters for browser clients. Empty = do not emit a wildcard
    # Access-Control-Allow-Origin. Set to a specific https origin if needed.
    api_cors_origin = "",
    # If set, the API rejects any request lacking a matching X-Proxy-Secret
    # header. Set the same value in the reverse proxy so only it can reach the API.
    proxy_shared_secret = "",
    # Envelope-encrypt job payloads/results at rest with per-user keys derived
    # from the API token (requires the 'sodium' package). Strongly recommended
    # for GDPR-sensitive data. Disable only for non-sensitive test deployments.
    encrypt_at_rest = TRUE,
    launcher = "local",
    docker_image = "liberties-worker:latest",
    worker_rscript = Sys.which("Rscript"),
    liberation_root = "",
    libertad_root = "",
    # FALSE lets each user run up to their max_concurrent_jobs in parallel. Set
    # TRUE only if the native estimation engine must never run concurrently.
    worker_serial_native = FALSE,
    # Server-wide cap on simultaneously running jobs across ALL users (0 =
    # unlimited). Protects the host from CPU/RAM oversubscription; per-user
    # max_concurrent_jobs / max_cpu / max_memory_mb remain the primary throttle.
    max_global_running = 0L,
    max_queue = 100L,
    # Server-side caps on estimation resource parameters (reject at submit if exceeded).
    max_est_maxit = 5000L,
    max_est_max_outer = 50L,
    max_est_n_iter = 10000L,
    max_est_n_burn = 5000L,
    max_est_n_mcmc = 50000L,
    max_est_n_quad = 20L,
    max_est_n_imp = 5000L,
    max_est_bootstrap_n = 500L,
    # When TRUE, refuse to start the API unless proxy_shared_secret is configured.
    require_proxy_secret = FALSE
  )
}

#' @keywords internal
.ls_default_sandbox_root <- function() {
  if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA", unset = path.expand("~")),
              "LibeRties", "sandbox")
  } else {
    "/var/lib/liberties"
  }
}

#' Is a path inside an R per-session temp directory (e.g. .../RtmpXXXX/...)?
#' @keywords internal
.ls_is_ephemeral_path <- function(p) {
  p <- as.character(p %||% "")
  if (!nzchar(p)) {
    return(FALSE)
  }
  grepl("[/\\\\]Rtmp[A-Za-z0-9]+([/\\\\]|$)", p)
}

#' Should a persisted sandbox_root be ignored?
#'
#' A sandbox_root that lives in an R session temp dir AND no longer exists is a
#' dead pointer from a previous session (e.g. a test/debug run that set a
#' throwaway sandbox via ls_config_set). Honoring it would silently create an
#' empty sandbox and hide all existing users, so we fall back to the default.
#' A temp path that still exists (e.g. the current test session) is honored.
#' @keywords internal
.ls_stale_sandbox_root <- function(p) {
  p <- as.character(p %||% "")
  nzchar(p) && .ls_is_ephemeral_path(p) && !dir.exists(p)
}

#' Read env var, with optional legacy fallback name
#' @keywords internal
.ls_env <- function(name, legacy = NULL, unset = "") {
  val <- Sys.getenv(name, unset = "")
  if (nzchar(val)) {
    return(val)
  }
  if (!is.null(legacy) && nzchar(legacy)) {
    val <- Sys.getenv(legacy, unset = "")
    if (nzchar(val)) {
      return(val)
    }
  }
  unset
}

#' Resolve sandbox root without calling ls_config() (avoids recursion)
#' @keywords internal
.ls_resolve_sandbox_root <- function() {
  root_env <- .ls_env("LIBERTIES_SANDBOX_ROOT", "LIBERATION_SANDBOX_ROOT")
  if (nzchar(root_env)) {
    return(normalizePath(root_env, winslash = "/", mustWork = FALSE))
  }
  cached <- .ls_pkg_env$config
  if (!is.null(cached) && !is.null(cached$sandbox_root)) {
    return(cached$sandbox_root)
  }
  default_root <- .ls_default_sandbox_root()
  cfg_path <- file.path(default_root, "config.json")
  if (file.exists(cfg_path)) {
    file_cfg <- tryCatch(
      jsonlite::read_json(cfg_path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.null(file_cfg$sandbox_root) && nzchar(as.character(file_cfg$sandbox_root)) &&
        !.ls_stale_sandbox_root(file_cfg$sandbox_root)) {
      return(normalizePath(as.character(file_cfg$sandbox_root), winslash = "/", mustWork = FALSE))
    }
  }
  default_root
}

#' @keywords internal
.ls_path <- function(...) {
  file.path(.ls_resolve_sandbox_root(), ...)
}

#' @keywords internal
.ls_config_path <- function() {
  env <- .ls_env("LIBERTIES_CONFIG", "LIBERATION_SCHEDULER_CONFIG")
  if (nzchar(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  file.path(.ls_default_sandbox_root(), "config.json")
}

#' Drop a persisted sandbox_root that points at a dead R session temp dir.
#' @keywords internal
.ls_sanitize_file_cfg <- function(file_cfg) {
  if (!is.null(file_cfg$sandbox_root) && .ls_stale_sandbox_root(file_cfg$sandbox_root)) {
    file_cfg$sandbox_root <- NULL
  }
  file_cfg
}

#' @keywords internal
.ls_config_read_file <- function() {
  paths <- unique(c(
    .ls_config_path(),
    file.path(.ls_default_sandbox_root(), "config.json")
  ))
  for (path in paths) {
    if (!file.exists(path)) {
      next
    }
    file_cfg <- tryCatch(
      jsonlite::read_json(path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.null(file_cfg) && length(file_cfg) > 0L) {
      return(.ls_sanitize_file_cfg(file_cfg))
    }
  }
  legacy_root <- .ls_env("LIBERTIES_SANDBOX_ROOT", "LIBERATION_SANDBOX_ROOT")
  if (nzchar(legacy_root)) {
    legacy_path <- file.path(
      normalizePath(legacy_root, winslash = "/", mustWork = FALSE),
      "config.json"
    )
    if (file.exists(legacy_path)) {
      file_cfg <- tryCatch(
        jsonlite::read_json(legacy_path, simplifyVector = TRUE),
        error = function(e) NULL
      )
      if (!is.null(file_cfg) && length(file_cfg) > 0L) {
        return(.ls_sanitize_file_cfg(file_cfg))
      }
    }
  }
  list()
}

#' Resolve sandbox root directory
#'
#' @return Normalized path (created if missing).
#' @export
ls_sandbox_root <- function() {
  root <- .ls_resolve_sandbox_root()
  if (!dir.exists(root)) {
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

#' Read scheduler configuration
#'
#' Environment variables override file values:
#' \code{LIBERTIES_SANDBOX_ROOT}, \code{LIBERTIES_API_PORT},
#' \code{LIBERTIES_LAUNCHER}, \code{LIBERTIES_DOCKER_IMAGE}.
#' Legacy \code{LIBERATION_*} names are still accepted.
#'
#' @return Named list.
#' @export
ls_config <- function() {
  cached <- .ls_pkg_env$config
  if (!is.null(cached)) {
    return(cached)
  }
  cfg <- .ls_default_config()
  file_cfg <- .ls_config_read_file()
  if (length(file_cfg) > 0L) {
    cfg[names(file_cfg)] <- file_cfg
  }
  root_env <- .ls_env("LIBERTIES_SANDBOX_ROOT", "LIBERATION_SANDBOX_ROOT")
  if (nzchar(root_env)) {
    cfg$sandbox_root <- normalizePath(root_env, winslash = "/", mustWork = FALSE)
  }
  port_env <- .ls_env("LIBERTIES_API_PORT", "LIBERATION_API_PORT")
  if (nzchar(port_env)) {
    cfg$api_port <- as.integer(port_env)
  }
  launcher_env <- .ls_env("LIBERTIES_LAUNCHER", "LIBERATION_LAUNCHER")
  if (nzchar(launcher_env)) {
    cfg$launcher <- launcher_env
  }
  image_env <- .ls_env("LIBERTIES_DOCKER_IMAGE", "LIBERATION_DOCKER_IMAGE")
  if (nzchar(image_env)) {
    cfg$docker_image <- image_env
  }
  nm_root_env <- .ls_env("LIBERATION_ROOT", "LIBERATION_ROOT")
  if (nzchar(nm_root_env)) {
    cfg$liberation_root <- normalizePath(nm_root_env, winslash = "/", mustWork = FALSE)
  }
  ad_root_env <- .ls_env("LIBERTAD_ROOT", "LIBERTAD_ROOT")
  if (nzchar(ad_root_env)) {
    cfg$libertad_root <- normalizePath(ad_root_env, winslash = "/", mustWork = FALSE)
  }
  .ls_pkg_env$config <- cfg
  cfg
}

#' Clear cached configuration (for tests or after env changes)
#' @export
ls_config_reset <- function() {
  if (exists("config", envir = .ls_pkg_env, inherits = FALSE)) {
    rm("config", envir = .ls_pkg_env)
  }
  invisible(TRUE)
}

#' Reload scheduler configuration from disk
#'
#' Call after changing \code{config.json} or when admin/API processes may
#' have diverged.
#' @return Updated config (invisibly).
#' @export
ls_config_reload <- function() {
  ls_config_reset()
  ls_config()
}

#' Write scheduler configuration
#'
#' @param ... Named config fields to merge into the stored config.
#' @return Updated config (invisibly).
#' @export
ls_config_set <- function(...) {
  dots <- list(...)
  cfg <- ls_config()
  cfg[names(dots)] <- dots
  path <- .ls_config_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, pretty = TRUE)
  .ls_pkg_env$config <- cfg
  invisible(cfg)
}

#' Initialize scheduler storage layout
#' @keywords internal
.ls_init_storage <- function() {
  root <- ls_sandbox_root()
  dirs <- c(
    file.path(root, "datasets", "files"),
    file.path(root, "sandboxes"),
    file.path(root, "admin")
  )
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  # Owner-only ACLs on the sandbox root and sensitive subtrees so a co-tenant
  # OS user cannot read tenant data/keys off disk (complements encryption).
  .ls_secure_dir(root)
  .ls_secure_dir(file.path(root, "sandboxes"))
  .ls_secure_dir(file.path(root, "datasets"))
  .ls_secure_dir(file.path(root, "admin"))
  users_path <- .ls_users_path()
  if (!file.exists(users_path)) {
    jsonlite::write_json(list(), users_path, auto_unbox = TRUE, pretty = TRUE)
  }
  catalog_path <- .ls_datasets_catalog_path()
  if (!file.exists(catalog_path)) {
    jsonlite::write_json(list(), catalog_path, auto_unbox = TRUE, pretty = TRUE)
  }
  invisible(root)
}
