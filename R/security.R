.ls_loopback <- function(host) {
  tolower(as.character(host)) %in% c("127.0.0.1", "localhost", "::1")
}

#' Define a LibeRties deployment-security policy
#'
#' @param production Enable production requirements.
#' @param requests_per_minute Per-credential HTTP request ceiling.
#' @param require_storage_encryption Require `LIBERTIES_STORAGE_KEY`.
#' @param require_os_isolation Require `LIBERTIES_OS_ISOLATION` to identify the
#'   external container, service-account, cgroup, or Windows Job Object layer.
#' @return A validated `liberties_security_policy`.
#' @export
ls_security_policy <- function(
    production = FALSE, requests_per_minute = if (production) 120L else 10000L,
    require_storage_encryption = production,
    require_os_isolation = production) {
  requests_per_minute <- as.integer(requests_per_minute)
  if (length(requests_per_minute) != 1L || is.na(requests_per_minute) ||
      requests_per_minute < 1L) {
    .ls_stop("`requests_per_minute` must be a positive integer.")
  }
  structure(list(
    version = 1L, production = isTRUE(production),
    requests_per_minute = requests_per_minute,
    require_storage_encryption = isTRUE(require_storage_encryption),
    require_os_isolation = isTRUE(require_os_isolation)
  ), class = "liberties_security_policy")
}

#' Generate a LibeRties storage-encryption key
#'
#' The returned 64-character secret is shown once and should be placed in a
#' managed secret store as `LIBERTIES_STORAGE_KEY`, never in source control.
#' @export
ls_generate_storage_key <- function() .ls_random_hex(32L)

#' Check whether a LibeRties deployment satisfies its security policy
#'
#' @param root Server storage root.
#' @param host Intended listen address.
#' @param behind_tls_proxy Whether a maintained TLS reverse proxy terminates
#'   HTTPS before traffic reaches LibeRties.
#' @param policy A [ls_security_policy()] object.
#' @param strict Throw an error for unmet requirements.
#' @return A `liberties_security_preflight` report.
#' @export
ls_server_preflight <- function(
    root = .ls_default_root(), host = "127.0.0.1", behind_tls_proxy = FALSE,
    policy = ls_security_policy(production = !.ls_loopback(host)), strict = FALSE) {
  if (!inherits(policy, "liberties_security_policy")) {
    .ls_stop("`policy` must be created by ls_security_policy().")
  }
  root <- .ls_ensure_dir(root)
  issues <- warnings <- character()
  if (!.ls_loopback(host) && !isTRUE(behind_tls_proxy)) {
    issues <- c(issues, "A non-loopback service requires a maintained TLS reverse proxy.")
  }
  encrypted <- !is.null(.ls_storage_key())
  if (policy$require_storage_encryption && !encrypted) {
    issues <- c(issues, "Production storage encryption requires LIBERTIES_STORAGE_KEY.")
  }
  os_isolation <- trimws(Sys.getenv("LIBERTIES_OS_ISOLATION", unset = ""))
  if (policy$require_os_isolation && !nzchar(os_isolation)) {
    issues <- c(issues, paste(
      "Production execution requires an external OS isolation layer; set",
      "LIBERTIES_OS_ISOLATION to its managed deployment identifier."
    ))
  }
  users <- .ls_registry_load(root)
  non_expiring <- names(Filter(function(user) !nzchar(user$expires %||% ""), users))
  if (policy$production && length(non_expiring)) {
    warnings <- c(warnings, paste0("Non-expiring API tokens: ",
                                  paste(non_expiring, collapse = ", "), "."))
  }
  warnings <- c(warnings,
                "Worker stdout/stderr logs are plaintext; keep logs free of patient-level values.")
  report <- structure(list(
    ready = !length(issues), production = policy$production, root = root,
    host = as.character(host), tls_proxy = isTRUE(behind_tls_proxy),
    storage_encrypted = encrypted,
    os_isolation = if (nzchar(os_isolation)) os_isolation else "restricted subprocess only",
    requests_per_minute = policy$requests_per_minute,
    issues = issues, warnings = warnings, checked = .ls_now()
  ), class = "liberties_security_preflight")
  if (isTRUE(strict) && length(issues)) .ls_stop(paste(issues, collapse = " "))
  report
}

#' @export
print.liberties_security_preflight <- function(x, ...) {
  cat("LibeRties security preflight\n")
  cat("  ready:", x$ready, " production:", x$production,
      " encrypted storage:", x$storage_encrypted, "\n")
  if (length(x$issues)) cat(paste0("  ERROR: ", x$issues, collapse = "\n"), "\n")
  if (length(x$warnings)) cat(paste0("  WARN: ", x$warnings, collapse = "\n"), "\n")
  invisible(x)
}
