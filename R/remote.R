.ls_bearer_token <- function(req) {
  authorization <- as.character(req$HTTP_AUTHORIZATION %||% "")
  if (!nzchar(authorization) && !is.null(req$headers)) {
    names <- names(req$headers)
    match <- which(tolower(names %||% character()) == "authorization")
    if (length(match)) authorization <- as.character(req$headers[[match[[1L]]]])
  }
  if (!grepl("^Bearer[[:space:]]+", authorization, ignore.case = TRUE)) {
    .ls_stop("Unauthorized: missing bearer token.")
  }
  trimws(sub("^Bearer[[:space:]]+", "", authorization, ignore.case = TRUE))
}

.ls_api_error <- function(res, error) {
  message <- conditionMessage(error)
  res$status <- if (grepl("^Unauthorized:", message)) 401L else if (
    grepl("^Unknown job id", message)
  ) 404L else if (
    grepl("limit|quota|no completed result| is (queued|running|failed|cancelled)",
          message, ignore.case = TRUE)
  ) 409L else 400L
  list(error = message)
}

.ls_api_jobs <- function(jobs) {
  if (!nrow(jobs)) return(list())
  unname(lapply(seq_len(nrow(jobs)), function(i) as.list(jobs[i, , drop = FALSE])))
}

#' Build the authenticated LibeRties HTTP API
#'
#' The API accepts only the typed JSON wire contract. It does not expose an RDS
#' deserializer or an endpoint capable of evaluating submitted R code.
#'
#' @param server A [LibeRServer] instance.
#' @param policy HTTP security and rate-limit policy.
#' @return A configured Plumber router.
#' @export
ls_api <- function(server = ls_server(), policy = ls_security_policy()) {
  if (!inherits(server, "LibeRServer")) .ls_stop("`server` must be a LibeRServer.")
  if (!inherits(policy, "liberties_security_policy")) {
    .ls_stop("`policy` must be created by ls_security_policy().")
  }
  serializer <- plumber::serializer_unboxed_json(digits = 17, null = "null")
  api <- plumber::pr()
  rate_state <- new.env(parent = emptyenv())
  api <- plumber::pr_set_error(api, function(req, res, err) .ls_api_error(res, err))
  api <- plumber::pr_filter(api, "security_headers", function(req, res) {
    res$setHeader("Cache-Control", "no-store")
    res$setHeader("X-Content-Type-Options", "nosniff")
    res$setHeader("X-Frame-Options", "DENY")
    res$setHeader("Referrer-Policy", "no-referrer")
    res$setHeader("Content-Security-Policy", "default-src 'none'; frame-ancestors 'none'")
    if (isTRUE(policy$production)) {
      res$setHeader("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
    }
    plumber::forward()
  })
  api <- plumber::pr_filter(api, "rate_limit", function(req, res) {
    credential <- tryCatch(.ls_bearer_token(req), error = function(error) "anonymous")
    key <- if (identical(credential, "anonymous")) {
      paste0("anonymous:", req$REMOTE_ADDR %||% "unknown")
    } else paste0("token:", .ls_token_hash(credential))
    bucket <- floor(as.numeric(Sys.time()) / 60)
    state <- if (exists(key, rate_state, inherits = FALSE)) get(key, rate_state) else
      list(bucket = bucket, count = 0L)
    if (!identical(state$bucket, bucket)) state <- list(bucket = bucket, count = 0L)
    state$count <- state$count + 1L
    assign(key, state, rate_state)
    res$setHeader("X-RateLimit-Limit", as.character(policy$requests_per_minute))
    res$setHeader("X-RateLimit-Remaining",
                  as.character(max(0L, policy$requests_per_minute - state$count)))
    if (state$count > policy$requests_per_minute) {
      res$status <- 429L
      return(list(error = "Request rate limit exceeded."))
    }
    plumber::forward()
  })
  api <- plumber::pr_get(api, "/v1/health", function() {
    list(status = "ok", contract = "liber.job.wire/2", result_contract = "liber.result.wire/2",
         time = .ls_now())
  }, serializer = serializer)
  api <- plumber::pr_get(api, "/v1/auth", function(req) {
    auth <- server$authenticate(.ls_bearer_token(req))
    list(username = auth$username, limits = auth$limits)
  }, serializer = serializer)
  api <- plumber::pr_post(api, "/v1/jobs", function(req) {
    token <- .ls_bearer_token(req)
    auth <- server$authenticate(token)
    job <- ls_job_decode(req$postBody %||% "", auth$limits$max_payload_mb * 1024^2)
    list(id = server$submit(token, job), status = "queued")
  }, serializer = serializer)
  api <- plumber::pr_get(api, "/v1/jobs", function(req) {
    token <- .ls_bearer_token(req)
    server$poll(token)
    list(jobs = .ls_api_jobs(server$list(token)))
  }, serializer = serializer)
  api <- plumber::pr_get(api, "/v1/jobs/<id>", function(req, id) {
    token <- .ls_bearer_token(req)
    server$poll(token)
    server$status(token, id)
  }, serializer = serializer)
  api <- plumber::pr_get(api, "/v1/jobs/<id>/result", function(req, id) {
    ls_result_to_wire(server$result(.ls_bearer_token(req), id))
  }, serializer = serializer)
  api <- plumber::pr_get(api, "/v1/jobs/<id>/logs", function(req, id,
                                                               stream = "stdout") {
    list(lines = server$logs(.ls_bearer_token(req), id, stream = stream))
  }, serializer = serializer)
  api <- plumber::pr_delete(api, "/v1/jobs/<id>", function(req, id) {
    list(cancelled = server$cancel(.ls_bearer_token(req), id))
  }, serializer = serializer)
  api
}

#' Run the LibeRties HTTP service
#'
#' Bind to loopback by default and place a TLS reverse proxy in front of this
#' service for remote deployment.
#'
#' @param root Persistent server root.
#' @param host Listen address.
#' @param port TCP port.
#' @param max_workers_per_user Host-level per-user worker ceiling.
#' @param quiet Suppress Plumber startup messages.
#' @param production Enforce production preflight. Defaults to `TRUE` for a
#'   non-loopback host.
#' @param behind_tls_proxy Confirm that HTTPS is terminated by a maintained
#'   reverse proxy.
#' @param policy Optional security policy.
#' @export
ls_run_api <- function(root = .ls_default_root(), host = "127.0.0.1", port = 8000L,
                       max_workers_per_user = 2L, quiet = FALSE,
                       production = !.ls_loopback(host), behind_tls_proxy = FALSE,
                       policy = ls_security_policy(production = production)) {
  ls_server_preflight(root, host, behind_tls_proxy, policy, strict = isTRUE(production))
  api <- ls_api(ls_server(root, max_workers_per_user), policy = policy)
  api$run(host = host, port = as.integer(port), swagger = FALSE, quiet = quiet)
}

.ls_remote_call <- function(remote, method, path, body = NULL) {
  request <- httr2::request(paste0(remote$url, path))
  request <- httr2::req_headers(request, Authorization = paste("Bearer", remote$token))
  request <- httr2::req_timeout(request, remote$timeout)
  request <- httr2::req_method(request, method)
  if (!is.null(body)) request <- httr2::req_body_json(request, body, auto_unbox = TRUE)
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  response <- httr2::req_perform(request)
  if (httr2::resp_status(response) >= 400L) {
    error <- tryCatch(
      httr2::resp_body_json(response, simplifyVector = FALSE)$error,
      error = function(e) paste("HTTP", httr2::resp_status(response))
    )
    .ls_stop(as.character(error %||% paste("HTTP", httr2::resp_status(response))))
  }
  httr2::resp_body_json(response, simplifyVector = FALSE)
}

#' LibeRties remote-server client
#'
#' @export
LibeRRemote <- R6::R6Class(
  "LibeRRemote",
  public = list(
    #' @field url Normalized HTTP(S) service base URL.
    url = NULL,
    #' @field token Bearer token used for authenticated requests.
    token = NULL,
    #' @field timeout Per-request timeout in seconds.
    timeout = NULL,

    #' @description
    #' Configure an authenticated remote client.
    #' @param url HTTP(S) service base URL.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param timeout Per-request timeout in seconds.
    #' @return A new `LibeRRemote` object.
    initialize = function(url, token, timeout = 60) {
      self$url <- sub("/+$", "", as.character(url))
      self$token <- as.character(token)
      self$timeout <- as.numeric(timeout)
      if (length(self$url) != 1L || !grepl("^https?://", self$url) ||
          length(self$token) != 1L || !nzchar(self$token) ||
          length(self$timeout) != 1L || !is.finite(self$timeout) || self$timeout <= 0) {
        .ls_stop("Remote URL, token, or timeout is invalid.")
      }
    },

    #' @description
    #' Verify the bearer token and return the authenticated user metadata.
    #' @return Server authentication metadata.
    authenticate = function() .ls_remote_call(self, "GET", "/v1/auth"),

    #' @description
    #' Submit a typed, non-executable job payload.
    #' @param job A job created by [ls_job()].
    #' @return The durable remote job identifier.
    submit = function(job) {
      response <- .ls_remote_call(self, "POST", "/v1/jobs", ls_job_to_wire(job))
      as.character(response$id)
    },

    #' @description
    #' List jobs belonging to the authenticated user.
    #' @return A data frame of remote jobs.
    list = function() {
      jobs <- .ls_remote_call(self, "GET", "/v1/jobs")$jobs
      if (!length(jobs)) return(.ls_empty_jobs())
      result <- do.call(rbind, lapply(jobs, function(x) {
        as.data.frame(lapply(x, unlist, use.names = FALSE), stringsAsFactors = FALSE)
      }))
      rownames(result) <- NULL
      result
    },

    #' @description
    #' Read the status and metadata of one remote job.
    #' @param id Durable remote job identifier.
    #' @return A named metadata list.
    status = function(id) {
      .ls_remote_call(self, "GET", paste0("/v1/jobs/", utils::URLencode(id, reserved = TRUE)))
    },

    #' @description
    #' Download and validate a completed result.
    #' @param id Durable remote job identifier.
    #' @return The reconstructed result object.
    result = function(id) {
      payload <- .ls_remote_call(
        self, "GET", paste0("/v1/jobs/", utils::URLencode(id, reserved = TRUE), "/result")
      )
      ls_result_from_wire(payload)
    },

    #' @description
    #' Download a remote worker log stream.
    #' @param id Durable remote job identifier.
    #' @param stream Standard-output or standard-error stream.
    #' @return A character vector containing log lines.
    logs = function(id, stream = c("stdout", "stderr")) {
      stream <- match.arg(stream)
      payload <- .ls_remote_call(
        self, "GET", paste0("/v1/jobs/", utils::URLencode(id, reserved = TRUE),
                            "/logs?stream=", stream)
      )
      .ls_wire_character(payload$lines)
    },

    #' @description
    #' Request cancellation of a queued or running remote job.
    #' @param id Durable remote job identifier.
    #' @return `TRUE` if the server accepted cancellation.
    cancel = function(id) {
      payload <- .ls_remote_call(
        self, "DELETE", paste0("/v1/jobs/", utils::URLencode(id, reserved = TRUE))
      )
      isTRUE(payload$cancelled)
    }
  )
)

#' Connect to a LibeRties remote server
#' @param url Server base URL.
#' @param token Bearer token issued by [ls_user_create()].
#' @param timeout HTTP timeout in seconds.
#' @return A configured `LibeRRemote` client.
#' @examples
#' remote <- ls_remote("https://liberties.example.org", "replace-with-token")
#' remote$url
#' @export
ls_remote <- function(url, token, timeout = 60) LibeRRemote$new(url, token, timeout)
