#' @keywords internal
.ls_req_header <- function(req, name) {
  if (identical(tolower(name), "authorization")) {
    auth <- req$HTTP_AUTHORIZATION %||% ""
    if (nzchar(auth)) {
      return(as.character(auth))
    }
  } else {
    env_name <- paste0("HTTP_", gsub("-", "_", toupper(name)))
    val <- req[[env_name]] %||% ""
    if (nzchar(val)) {
      return(as.character(val))
    }
  }
  headers <- req$headers
  if (is.null(headers)) {
    return("")
  }
  if (!is.null(headers[[name]])) {
    return(as.character(headers[[name]]))
  }
  nms <- names(headers)
  if (is.null(nms)) {
    return("")
  }
  hit <- nms[tolower(nms) == tolower(name)]
  if (length(hit) > 0L) {
    return(as.character(headers[[hit[[1L]]]]))
  }
  ""
}

#' @keywords internal
.ls_bearer_token <- function(req) {
  token <- .ls_req_header(req, "X-API-Token")
  if (!nzchar(token)) {
    auth <- .ls_req_header(req, "Authorization")
    if (grepl("^Bearer[[:space:]]+", auth, ignore.case = TRUE)) {
      token <- sub("^Bearer[[:space:]]+", "", auth, ignore.case = TRUE)
    } else if (nzchar(auth)) {
      token <- auth
    }
  }
  trimws(as.character(token))
}

#' @keywords internal
.ls_admin_token <- function(req) {
  trimws(as.character(.ls_req_header(req, "X-Admin-Token")))
}

#' @keywords internal
.ls_require_user <- function(req) {
  token <- .ls_bearer_token(req)
  if (!nzchar(token)) {
    stop("Unauthorized: missing API token.", call. = FALSE)
  }
  user <- .ls_user_from_token(token)
  if (is.null(user)) {
    stop("Unauthorized: invalid API token.", call. = FALSE)
  }
  # Derive and cache the user's at-rest encryption key from the live token so
  # subsequent submit/launch/result operations in this (and near-future)
  # requests can wrap/unwrap the per-job DEK. Never persisted to disk.
  if (.ls_encryption_enabled()) {
    tryCatch(.ls_uk_remember(user$username, token), error = function(e) NULL)
  }
  user
}

#' @keywords internal
.ls_require_admin <- function(req) {
  token <- .ls_admin_token(req)
  if (!.ls_admin_token_verify(token)) {
    stop("Admin access denied.", call. = FALSE)
  }
  TRUE
}

#' Test whether an API token matches a LibeRties user
#'
#' Useful for debugging client/server auth mismatches from the server console.
#'
#' @param token Plain API token (e.g. \code{lr_...}).
#' @return List with \code{ok}, \code{username}, sandbox path, and user count.
#' @export
ls_auth_test_token <- function(token) {
  token <- trimws(as.character(token))
  users <- .ls_users_load()
  info <- list(
    sandbox = ls_sandbox_root(),
    users_file = .ls_users_path(),
    n_users = length(users),
    usernames = names(users)
  )
  if (!nzchar(token)) {
    stop("Token is empty.", call. = FALSE)
  }
  user <- .ls_user_from_token(token)
  if (is.null(user)) {
    stop("Token not recognized.", call. = FALSE)
  }
  c(info, list(ok = TRUE, username = user$username))
}

#' @keywords internal
.ls_json_response <- function(x, status = 200L) {
  list(
    body = jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"),
    status = status,
    headers = list("Content-Type" = "application/json")
  )
}

#' @keywords internal
.ls_error_response <- function(msg, status = 400L) {
  .ls_json_response(list(error = msg), status = status)
}
