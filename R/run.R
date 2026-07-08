#' Start the scheduler Plumber API
#'
#' @param host Host to bind (default from config).
#' @param port Port (default from config).
#' @export
ls_run_api <- function(host = NULL, port = NULL) {
  cfg <- ls_config()
  .ls_init_storage()
  if (isTRUE(cfg$require_proxy_secret)) {
    secret <- as.character(cfg$proxy_shared_secret %||% "")
    if (!nzchar(secret)) {
      stop(
        "require_proxy_secret is TRUE but proxy_shared_secret is empty. ",
        "Configure a shared secret before starting the API.",
        call. = FALSE
      )
    }
  }
  host <- host %||% cfg$api_host
  port <- port %||% cfg$api_port
  api_file <- system.file("plumber", "api.R", package = "LibeRties")
  if (!nzchar(api_file)) {
    stop("API definition not found.", call. = FALSE)
  }
  pr <- plumber::plumb(api_file)
  pr <- plumber::pr_set_error(pr, .ls_plumber_error_handler)
  info <- .ls_startup_info()
  port <- as.integer(port)
  browse_host <- if (identical(host, "0.0.0.0")) "127.0.0.1" else host
  health_url <- paste0("http://", browse_host, ":", port, "/v1/health")
  message("LibeRties API on http://", host, ":", port)
  if (identical(host, "0.0.0.0") || identical(host, "::")) {
    message(
      "SECURITY WARNING: API is bound to all interfaces without TLS. For ",
      "GDPR-sensitive data, put a TLS reverse proxy in front (see ",
      "inst/deploy/ for Caddy/nginx configs), set api_host to \"127.0.0.1\", ",
      "and connect clients over https://."
    )
  }
  message("Health check: ", health_url)
  message("Admin GUI: ls_run_admin()  (this API has no HTML UI)")
  message("Sandbox: ", info$sandbox)
  message("Users file: ", info$users_file, " (", info$n_users, " user(s))")
  worker_env <- .ls_job_worker_env()
  if (identical(worker_env$mode, "dev")) {
    message("Worker packages: dev load from ", worker_env$nm_root)
  } else if (requireNamespace("LibeRation", quietly = TRUE)) {
    message("Worker packages: installed LibeRation")
  } else {
    message(
      "Worker packages: LibeRation not installed — set LIBERATION_ROOT or liberation_root in config."
    )
  }
  if (info$n_users == 0L) {
    message("No users yet — create one with ls_user_create() or ls_run_admin().")
  }
  plumber::pr_run(pr, host = host, port = port)
}

#' Launch the admin Shiny application
#'
#' @param host Host (default from config \code{admin_host}).
#' @param port Port (default from config \code{admin_port}).
#' @export
ls_run_admin <- function(host = NULL, port = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install package 'shiny' to run the admin app.", call. = FALSE)
  }
  cfg <- ls_config()
  .ls_init_storage()
  host <- host %||% cfg$admin_host
  port <- port %||% cfg$admin_port
  app_file <- system.file("admin-shiny", "app.R", package = "LibeRties")
  if (!nzchar(app_file)) {
    stop("Admin app not found.", call. = FALSE)
  }
  message("LibeRties admin on http://", host, ":", port)
  info <- .ls_startup_info()
  message("Sandbox: ", info$sandbox)
  message("Users file: ", info$users_file, " (", info$n_users, " user(s))")
  shiny::runApp(app_file, host = host, port = as.integer(port))
}
