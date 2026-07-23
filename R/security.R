.ls_loopback <- function(host) {
  tolower(as.character(host)) %in% c("127.0.0.1", "localhost", "::1")
}

.ls_clean_address <- function(value) {
  value <- trimws(as.character(value %||% ""))
  value <- sub("^for=", "", value, ignore.case = TRUE)
  value <- gsub('^"|"$', "", value)
  if (grepl("^\\[[^]]+\\](:[0-9]+)?$", value)) {
    return(sub("^\\[([^]]+)\\].*$", "\\1", value))
  }
  if (grepl("^[0-9.]+:[0-9]+$", value)) value <- sub(":[0-9]+$", "", value)
  value
}

.ls_ipv4_number <- function(value) {
  pieces <- suppressWarnings(as.integer(strsplit(value, ".", fixed = TRUE)[[1L]]))
  if (length(pieces) != 4L || anyNA(pieces) || any(pieces < 0L | pieces > 255L)) {
    return(NA_real_)
  }
  sum(pieces * 256^(3:0))
}

.ls_address_matches <- function(address, rule) {
  address <- .ls_clean_address(address)
  rule <- trimws(as.character(rule))
  if (!grepl("/", rule, fixed = TRUE)) {
    return(identical(tolower(address), tolower(.ls_clean_address(rule))))
  }
  pieces <- strsplit(rule, "/", fixed = TRUE)[[1L]]
  if (length(pieces) != 2L) return(FALSE)
  prefix <- suppressWarnings(as.integer(pieces[[2L]]))
  network <- .ls_ipv4_number(pieces[[1L]])
  candidate <- .ls_ipv4_number(address)
  if (!is.finite(network) || !is.finite(candidate) || is.na(prefix) ||
      prefix < 0L || prefix > 32L) return(FALSE)
  block <- 2^(32L - prefix)
  floor(network / block) == floor(candidate / block)
}

.ls_trusted_address <- function(address, trusted_proxies) {
  length(trusted_proxies) && any(vapply(
    trusted_proxies, function(rule) .ls_address_matches(address, rule), logical(1)
  ))
}

.ls_default_isolation_probe <- function() {
  if (identical(Sys.info()[["sysname"]], "Linux")) {
    cgroup_paths <- c("/proc/self/cgroup", "/proc/1/cgroup")
    cgroup <- unlist(lapply(cgroup_paths[file.exists(cgroup_paths)], function(path) {
      tryCatch(readLines(path, warn = FALSE), error = function(error) character())
    }), use.names = FALSE)
    marker <- file.exists("/.dockerenv") || file.exists("/run/.containerenv")
    pattern <- "docker|containerd|kubepods|libpod|lxc|podman|machine.slice"
    matched <- grep(pattern, cgroup, value = TRUE, ignore.case = TRUE)
    if (marker || length(matched)) {
      return(list(
        active = TRUE, provider = "linux-container-or-cgroup",
        evidence = c(if (marker) "container marker present", utils::head(matched, 3L))
      ))
    }
  }
  list(
    active = FALSE, provider = "none-detected",
    evidence = "No kernel/container isolation evidence was detected for this process."
  )
}

.ls_isolation_result <- function(probe = NULL) {
  result <- tryCatch(
    if (is.null(probe)) .ls_default_isolation_probe() else {
      if (!is.function(probe)) .ls_stop("`isolation_probe` must be a function or NULL.")
      probe()
    },
    error = function(error) list(
      active = FALSE, provider = "probe-error", evidence = conditionMessage(error)
    )
  )
  if (!is.list(result) || length(result$active) != 1L) {
    return(list(active = FALSE, provider = "invalid-probe",
                evidence = "Isolation probe returned an invalid result."))
  }
  list(
    active = isTRUE(result$active),
    provider = as.character(result$provider %||% "unspecified")[[1L]],
    evidence = as.character(result$evidence %||% character())
  )
}

#' Define a LibeRties deployment-security policy
#'
#' @param production Enable production requirements.
#' @param requests_per_minute Per-credential HTTP request ceiling.
#' @param max_rate_limit_keys Maximum live credential/address buckets retained
#'   by one API process. Additional new identities share a bounded overflow
#'   bucket until the next minute.
#' @param trusted_proxies Exact proxy addresses or IPv4 CIDR ranges whose
#'   `X-Forwarded-For` header may be trusted. The default trusts none.
#' @param max_log_lines,max_log_bytes Response ceilings for remote worker logs.
#' @param require_storage_encryption Require `LIBERTIES_STORAGE_KEY`.
#' @param require_os_isolation Require independently probed container/cgroup or
#'   deployment-provided OS isolation evidence. A descriptive environment
#'   variable alone is not accepted as proof.
#' @return A validated `liberties_security_policy`.
#' @export
ls_security_policy <- function(
    production = FALSE, requests_per_minute = if (production) 120L else 10000L,
    max_rate_limit_keys = 10000L, trusted_proxies = character(),
    max_log_lines = 5000L, max_log_bytes = 1024^2,
    require_storage_encryption = production,
    require_os_isolation = production) {
  positive_integer <- function(value, name) {
    value <- as.integer(value)
    if (length(value) != 1L || is.na(value) || value < 1L) {
      .ls_stop("`", name, "` must be a positive integer.")
    }
    value
  }
  requests_per_minute <- positive_integer(requests_per_minute, "requests_per_minute")
  max_rate_limit_keys <- positive_integer(max_rate_limit_keys, "max_rate_limit_keys")
  max_log_lines <- positive_integer(max_log_lines, "max_log_lines")
  max_log_bytes <- positive_integer(max_log_bytes, "max_log_bytes")
  trusted_proxies <- unique(trimws(as.character(trusted_proxies)))
  trusted_proxies <- trusted_proxies[nzchar(trusted_proxies)]
  if (any(vapply(trusted_proxies, function(rule) {
    if (!grepl("/", rule, fixed = TRUE)) return(!nzchar(.ls_clean_address(rule)))
    pieces <- strsplit(rule, "/", fixed = TRUE)[[1L]]
    prefix <- suppressWarnings(as.integer(pieces[[2L]]))
    length(pieces) != 2L || !is.finite(.ls_ipv4_number(pieces[[1L]])) ||
      is.na(prefix) || prefix < 0L || prefix > 32L
  }, logical(1)))) .ls_stop("`trusted_proxies` contains an invalid address or IPv4 CIDR rule.")
  structure(list(
    version = 2L, production = isTRUE(production),
    requests_per_minute = requests_per_minute,
    max_rate_limit_keys = max_rate_limit_keys,
    trusted_proxies = trusted_proxies,
    max_log_lines = max_log_lines,
    max_log_bytes = max_log_bytes,
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
#' @param isolation_probe Optional deployment-integrated function returning a
#'   list with logical `active`, `provider`, and `evidence`. When omitted,
#'   LibeRties checks Linux container/cgroup evidence. This should be connected
#'   to the actual service manager or sandbox on other platforms.
#' @return A `liberties_security_preflight` report.
#' @export
ls_server_preflight <- function(
    root = .ls_default_root(), host = "127.0.0.1", behind_tls_proxy = FALSE,
    policy = ls_security_policy(production = !.ls_loopback(host)), strict = FALSE,
    isolation_probe = NULL) {
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
  isolation <- .ls_isolation_result(isolation_probe)
  isolation_label <- trimws(Sys.getenv("LIBERTIES_OS_ISOLATION", unset = ""))
  if (policy$require_os_isolation && !isTRUE(isolation$active)) {
    issues <- c(issues, paste(
      "Production execution requires verifiable external OS isolation.",
      "A label in LIBERTIES_OS_ISOLATION is descriptive only; connect",
      "`isolation_probe` to the actual container, cgroup, service account, or sandbox."
    ))
  }
  users <- .ls_registry_load(root)
  non_expiring <- names(Filter(function(user) !nzchar(user$expires %||% ""), users))
  if (policy$production && length(non_expiring)) {
    warnings <- c(warnings, paste0("Non-expiring API tokens: ",
                                  paste(non_expiring, collapse = ", "), "."))
  }
  if (!encrypted) {
    warnings <- c(warnings,
                  "Worker stdout/stderr logs remain plaintext without LIBERTIES_STORAGE_KEY.")
  } else {
    warnings <- c(warnings,
                  "Live logs are plaintext while a worker runs and are encrypted when it becomes terminal.")
  }
  report <- structure(list(
    ready = !length(issues), production = policy$production, root = root,
    host = as.character(host), tls_proxy = isTRUE(behind_tls_proxy),
    storage_encrypted = encrypted,
    os_isolation = isolation$provider,
    os_isolation_active = isolation$active,
    os_isolation_evidence = isolation$evidence,
    os_isolation_label = isolation_label,
    requests_per_minute = policy$requests_per_minute,
    max_rate_limit_keys = policy$max_rate_limit_keys,
    trusted_proxies = policy$trusted_proxies,
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
