.ls_registry_path <- function(root) {
  file.path(.ls_ensure_dir(file.path(root, "server")), "users.rds")
}

.ls_registry_load <- function(root) {
  path <- .ls_registry_path(root)
  if (!file.exists(path)) return(list())
  users <- .ls_read_rds(path)
  if (!is.list(users) || (length(users) && is.null(names(users)))) {
    .ls_stop("The LibeRties user registry is invalid.")
  }
  users
}

.ls_registry_save <- function(root, users) {
  .ls_atomic_save_rds(users, .ls_registry_path(root))
}

.ls_with_registry_lock <- function(root, operation, timeout = 5) {
  lock <- paste0(.ls_registry_path(root), ".lock")
  started <- proc.time()[["elapsed"]]
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) break
    if (proc.time()[["elapsed"]] - started >= timeout) {
      .ls_stop("Timed out acquiring the LibeRties user-registry lock.")
    }
    Sys.sleep(0.01)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  operation()
}

.ls_default_limits <- function() {
  list(
    max_concurrent_jobs = 2L,
    max_queued_jobs = 20L,
    max_payload_mb = 100,
    max_result_mb = 500,
    max_storage_mb = 5120,
    max_runtime_seconds = 86400,
    max_cpu_seconds = 86400,
    max_memory_mb = 4096
  )
}

.ls_limits <- function(limits = list(), base = .ls_default_limits()) {
  if (!is.list(limits) || (length(limits) && is.null(names(limits)))) {
    .ls_stop("`limits` must be a named list.")
  }
  unknown <- setdiff(names(limits), names(base))
  if (length(unknown)) .ls_stop("Unknown user limit(s): ", paste(unknown, collapse = ", "), ".")
  result <- utils::modifyList(base, limits)
  result$max_concurrent_jobs <- as.integer(result$max_concurrent_jobs)
  result$max_queued_jobs <- as.integer(result$max_queued_jobs)
  for (name in c("max_payload_mb", "max_result_mb", "max_storage_mb",
                 "max_runtime_seconds", "max_cpu_seconds", "max_memory_mb")) {
    result[[name]] <- as.numeric(result[[name]])
  }
  values <- unlist(result, use.names = FALSE)
  if (any(lengths(result) != 1L) || anyNA(values) || any(!is.finite(values)) || any(values <= 0)) {
    .ls_stop("All user limits must be finite positive scalar values.")
  }
  result
}

.ls_token <- function() {
  paste0("lr_", format(Sys.time(), "%Y%m%d", tz = "UTC"), "_", .ls_random_hex(32L))
}

.ls_person_name <- function(value, what) {
  value <- trimws(as.character(value %||% ""))
  if (length(value) != 1L || is.na(value) || nchar(value, type = "chars") > 128L ||
      grepl("[\r\n\t]", value)) {
    .ls_stop("`", what, "` must be one line containing at most 128 characters.")
  }
  value
}

.ls_scopes <- function(scopes = c("jobs:read", "jobs:write")) {
  allowed <- c("jobs:read", "jobs:write")
  scopes <- unique(trimws(as.character(scopes)))
  if (!length(scopes) || anyNA(scopes) || length(setdiff(scopes, allowed))) {
    .ls_stop("`scopes` must contain jobs:read and/or jobs:write.")
  }
  scopes
}

.ls_parse_time <- function(value) {
  if (inherits(value, "POSIXt")) return(as.POSIXct(value, tz = "UTC"))
  text <- as.character(value)
  parsed <- suppressWarnings(as.POSIXct(
    text, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  ))
  if (is.na(parsed)) parsed <- suppressWarnings(as.POSIXct(text, tz = "UTC"))
  parsed
}

.ls_expiry <- function(expires = NULL) {
  if (is.null(expires) || identical(expires, "")) return("")
  value <- .ls_parse_time(expires)
  if (length(value) != 1L || is.na(value) || value <= Sys.time()) {
    .ls_stop("`expires` must be a future date-time or NULL.")
  }
  format(value, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

.ls_token_hash <- function(token) {
  token <- as.character(token)
  if (length(token) != 1L || is.na(token)) return("")
  unname(paste0(openssl::sha256(charToRaw(enc2utf8(token)))))
}

.ls_constant_time_equal <- function(left, right) {
  left <- charToRaw(as.character(left %||% ""))
  right <- charToRaw(as.character(right %||% ""))
  if (length(left) != length(right)) return(FALSE)
  difference <- 0L
  for (i in seq_along(left)) {
    difference <- bitwOr(difference, bitwXor(as.integer(left[[i]]), as.integer(right[[i]])))
  }
  identical(difference, 0L)
}

.ls_authorize <- function(root, token, scope = NULL) {
  supplied <- .ls_token_hash(token)
  if (!nzchar(supplied)) .ls_stop("Unauthorized: missing API token.")
  users <- .ls_registry_load(root)
  match <- NULL
  for (username in names(users)) {
    if (.ls_constant_time_equal(users[[username]]$token_hash, supplied)) match <- username
  }
  if (is.null(match)) .ls_stop("Unauthorized: invalid API token.")
  user <- users[[match]]
  if (!isTRUE(user$enabled)) .ls_stop("Unauthorized: user account is disabled.")
  expires <- as.character(user$expires %||% "")
  expiry_time <- if (nzchar(expires)) .ls_parse_time(expires) else as.POSIXct(NA)
  if (nzchar(expires) && (is.na(expiry_time) || expiry_time <= Sys.time())) {
    .ls_stop("Unauthorized: API token has expired.")
  }
  scopes <- .ls_scopes(user$scopes %||% c("jobs:read", "jobs:write"))
  if (!is.null(scope) && !scope %in% scopes) {
    .ls_stop("Unauthorized: token scope is insufficient.")
  }
  list(username = match, limits = .ls_limits(user$limits %||% list()),
       created = user$created, scopes = scopes, expires = expires)
}

#' Create a server user and one-time bearer token
#'
#' Only the SHA-256 digest of the cryptographically random 256-bit token is
#' persisted. The returned token is the only copy available to the caller.
#'
#' @param root LibeRties server storage root.
#' @param username Tenant identifier.
#' @param limits Named resource limits.
#' @param enabled Whether the account may authenticate.
#' @param first_name,last_name Optional human-readable user names used by the
#'   administration interface. Authentication continues to use `username`.
#' @param scopes API permissions assigned to the token.
#' @param expires Optional future expiry date-time. `NULL` creates a non-expiring
#'   research token; production preflight reports it.
#' @return User metadata plus the one-time token.
#' @export
ls_user_create <- function(root, username, limits = list(), enabled = TRUE,
                           first_name = "", last_name = "",
                           scopes = c("jobs:read", "jobs:write"), expires = NULL) {
  root <- .ls_ensure_dir(root)
  username <- .ls_safe_component(username, "user id")
  limits <- .ls_limits(limits)
  first_name <- .ls_person_name(first_name, "first_name")
  last_name <- .ls_person_name(last_name, "last_name")
  scopes <- .ls_scopes(scopes)
  expires <- .ls_expiry(expires)
  created <- .ls_with_registry_lock(root, function() {
    users <- .ls_registry_load(root)
    if (!is.null(users[[username]])) .ls_stop("User already exists: ", username, ".")
    token <- .ls_token()
    users[[username]] <- list(
      token_hash = .ls_token_hash(token), enabled = isTRUE(enabled),
      first_name = first_name, last_name = last_name,
      scopes = scopes, expires = expires,
      limits = limits, created = .ls_now(), token_rotated = .ls_now()
    )
    .ls_registry_save(root, users)
    .ls_ensure_dir(file.path(root, "users", username, "jobs"))
    list(username = username, token = token, enabled = isTRUE(enabled),
         first_name = first_name, last_name = last_name, scopes = scopes,
         expires = expires, limits = limits)
  })
  .ls_audit_append(root, "user_created", username,
                   list(scopes = scopes, expires = expires, enabled = isTRUE(enabled)))
  created
}

#' List server users without credential material
#' @param root LibeRties server storage root.
#' @export
ls_user_list <- function(root) {
  users <- .ls_registry_load(.ls_ensure_dir(root))
  if (!length(users)) {
    return(data.frame(
      username = character(), first_name = character(), last_name = character(),
      enabled = logical(), created = character(), scopes = character(), expires = character(),
      max_concurrent_jobs = integer(), max_queued_jobs = integer(),
      max_payload_mb = numeric(), max_result_mb = numeric(), max_storage_mb = numeric(),
      max_runtime_seconds = numeric(), max_cpu_seconds = numeric(),
      max_memory_mb = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(names(users), function(username) {
    limits <- .ls_limits(users[[username]]$limits %||% list())
    data.frame(
      username = username,
      first_name = as.character(users[[username]]$first_name %||% ""),
      last_name = as.character(users[[username]]$last_name %||% ""),
      enabled = isTRUE(users[[username]]$enabled),
      created = users[[username]]$created,
      scopes = paste(.ls_scopes(users[[username]]$scopes %||%
                                c("jobs:read", "jobs:write")), collapse = ","),
      expires = as.character(users[[username]]$expires %||% ""),
      max_concurrent_jobs = limits$max_concurrent_jobs,
      max_queued_jobs = limits$max_queued_jobs,
      max_payload_mb = limits$max_payload_mb,
      max_result_mb = limits$max_result_mb,
      max_storage_mb = limits$max_storage_mb,
      max_runtime_seconds = limits$max_runtime_seconds,
      max_cpu_seconds = limits$max_cpu_seconds,
      max_memory_mb = limits$max_memory_mb,
      stringsAsFactors = FALSE
    )
  }))
}

#' Rotate a server user's bearer token
#' @param root LibeRties server storage root.
#' @param username Tenant identifier.
#' @param expires Optional replacement expiry date-time; `NULL` retains the
#'   current expiry.
#' @return New one-time bearer token.
#' @export
ls_user_rotate_token <- function(root, username, expires = NULL) {
  root <- .ls_ensure_dir(root)
  username <- .ls_safe_component(username, "user id")
  token <- .ls_with_registry_lock(root, function() {
    users <- .ls_registry_load(root)
    if (is.null(users[[username]])) .ls_stop("Unknown user: ", username, ".")
    token <- .ls_token()
    users[[username]]$token_hash <- .ls_token_hash(token)
    users[[username]]$token_rotated <- .ls_now()
    if (!is.null(expires)) users[[username]]$expires <- .ls_expiry(expires)
    .ls_registry_save(root, users)
    token
  })
  .ls_audit_append(root, "token_rotated", username,
                   list(expires = as.character(expires %||% "unchanged")))
  token
}

#' Update server-user status and resource limits
#' @param root LibeRties server storage root.
#' @param username Tenant identifier.
#' @param enabled Optional account state.
#' @param limits Optional named limit overrides.
#' @param first_name,last_name Optional human-readable names. Pass an empty
#'   string to clear a value and `NULL` to leave it unchanged.
#' @param scopes,expires Optional token permissions and expiry.
#' @export
ls_user_update <- function(root, username, enabled = NULL, limits = NULL,
                           first_name = NULL, last_name = NULL,
                           scopes = NULL, expires = NULL) {
  root <- .ls_ensure_dir(root)
  username <- .ls_safe_component(username, "user id")
  .ls_with_registry_lock(root, function() {
    users <- .ls_registry_load(root)
    user <- users[[username]]
    if (is.null(user)) .ls_stop("Unknown user: ", username, ".")
    if (!is.null(enabled)) user$enabled <- isTRUE(enabled)
    if (!is.null(limits)) user$limits <- .ls_limits(limits, .ls_limits(user$limits))
    if (!is.null(first_name)) user$first_name <- .ls_person_name(first_name, "first_name")
    if (!is.null(last_name)) user$last_name <- .ls_person_name(last_name, "last_name")
    if (!is.null(scopes)) user$scopes <- .ls_scopes(scopes)
    if (!is.null(expires)) user$expires <- .ls_expiry(expires)
    users[[username]] <- user
    .ls_registry_save(root, users)
    invisible(TRUE)
  })
  .ls_audit_append(root, "user_updated", username,
                   list(enabled = enabled, scopes = scopes, expires = expires))
  invisible(TRUE)
}

#' Delete a server user
#'
#' Active jobs must be cancelled or allowed to finish first. Job storage is
#' retained unless `remove_jobs = TRUE` is explicitly requested.
#'
#' @param root LibeRties server storage root.
#' @param username Tenant identifier.
#' @param remove_jobs Remove completed/cancelled/failed job storage as well.
#' @return `TRUE`, invisibly.
#' @export
ls_user_delete <- function(root, username, remove_jobs = FALSE) {
  root <- .ls_ensure_dir(root)
  username <- .ls_safe_component(username, "user id")
  deleted <- .ls_with_registry_lock(root, function() {
    users <- .ls_registry_load(root)
    if (is.null(users[[username]])) .ls_stop("Unknown user: ", username, ".")
    limits <- .ls_limits(users[[username]]$limits %||% list())
    queue <- LibeRQueue$new(root, username, max_workers = 1L, limits = limits)
    jobs <- queue$list()
    if (nrow(jobs) && any(!jobs$status %in% c("completed", "failed", "cancelled"))) {
      .ls_stop("User has active jobs; cancel them or wait for completion before deletion.")
    }
    user_path <- file.path(root, "users", username)
    if (dir.exists(user_path) && !isTRUE(remove_jobs)) {
      files <- list.files(user_path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
      if (length(files)) .ls_stop("User has retained job storage; set `remove_jobs = TRUE` to delete it.")
    }
    if (dir.exists(user_path) && isTRUE(remove_jobs)) {
      status <- unlink(user_path, recursive = TRUE, force = TRUE)
      if (!identical(status, 0L) || dir.exists(user_path)) {
        .ls_stop("Unable to remove storage for user ", username, ".")
      }
    }
    users[[username]] <- NULL
    .ls_registry_save(root, users)
    invisible(TRUE)
  })
  .ls_audit_append(root, "user_deleted", username, list(remove_jobs = isTRUE(remove_jobs)))
  deleted
}

.ls_storage_bytes <- function(root, username) {
  path <- file.path(root, "users", username)
  if (!dir.exists(path)) return(0)
  files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                      include.dirs = FALSE, no.. = TRUE)
  if (!length(files)) return(0)
  sum(file.info(files)$size, na.rm = TRUE)
}

#' Authenticated multi-tenant LibeRties server core
#'
#' This object is the transport-independent server boundary used by a local
#' test host and, later, the HTTP adapter. Every job operation derives the user
#' namespace from the bearer token; callers cannot nominate another tenant.
#'
#' @export
LibeRServer <- R6::R6Class(
  "LibeRServer",
  public = list(
    #' @field root Normalized persistent server storage directory.
    root = NULL,
    #' @field max_workers_per_user Host-level simultaneous-worker ceiling per tenant.
    max_workers_per_user = NULL,

    #' @description
    #' Create or reopen the transport-independent server core.
    #' @param root Persistent server storage directory.
    #' @param max_workers_per_user Host-level simultaneous-worker ceiling per tenant.
    #' @return A new `LibeRServer` object.
    initialize = function(root = .ls_default_root(), max_workers_per_user = 2L) {
      self$root <- .ls_ensure_dir(root)
      self$max_workers_per_user <- as.integer(max_workers_per_user)
      if (length(self$max_workers_per_user) != 1L || is.na(self$max_workers_per_user) ||
          self$max_workers_per_user < 1L) {
        .ls_stop("`max_workers_per_user` must be a positive integer.")
      }
      private$queues <- new.env(parent = emptyenv())
      .ls_registry_path(self$root)
    },

    #' @description
    #' Authenticate a bearer token without exposing credential material.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param scope Optional required API permission.
    #' @return User metadata and effective limits.
    authenticate = function(token, scope = NULL) .ls_authorize(self$root, token, scope),

    #' @description
    #' Submit a job to the authenticated user's isolated queue.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param job A job created by [ls_job()].
    #' @param start Start available work immediately.
    #' @return The durable job identifier, invisibly.
    submit = function(token, job, start = TRUE) {
      auth <- self$authenticate(token, "jobs:write")
      if (!inherits(job, "liber_job")) .ls_stop("`job` must be created by ls_job().")
      payload_bytes <- length(serialize(job, NULL, version = 3))
      if (payload_bytes > auth$limits$max_payload_mb * 1024^2) {
        .ls_stop("Payload exceeds this user's max_payload_mb limit.")
      }
      queue <- private$queue_for(auth)
      jobs <- queue$list()
      if (sum(jobs$status == "queued") >= auth$limits$max_queued_jobs) {
        .ls_stop("Queued-job limit reached for user ", auth$username, ".")
      }
      if (.ls_storage_bytes(self$root, auth$username) + payload_bytes >
          auth$limits$max_storage_mb * 1024^2) {
        .ls_stop("Storage quota reached for user ", auth$username, ".")
      }
      id <- queue$submit(job, start = start)
      .ls_audit_append(self$root, "job_submitted", auth$username,
                       list(id = id, type = job$type))
      id
    },

    #' @description
    #' Poll the authenticated user's queue.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param start Start available work immediately.
    #' @return The current job table, invisibly.
    poll = function(token, start = TRUE) {
      auth <- self$authenticate(token, "jobs:read")
      private$queue_for(auth)$poll(start = start)
    },

    #' @description
    #' List jobs for the authenticated user.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @return A data frame of durable jobs.
    list = function(token) {
      auth <- self$authenticate(token, "jobs:read")
      private$queue_for(auth)$list()
    },

    #' @description
    #' Read metadata for one authenticated-user job.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param id Durable job identifier.
    #' @return A named metadata list.
    status = function(token, id) {
      auth <- self$authenticate(token, "jobs:read")
      private$queue_for(auth)$status(id)
    },

    #' @description
    #' Read and size-check one completed result.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param id Durable job identifier.
    #' @return The deserialized result object.
    result = function(token, id) {
      auth <- self$authenticate(token, "jobs:read")
      result <- private$queue_for(auth)$result(id)
      bytes <- length(serialize(result, NULL, version = 3))
      if (bytes > auth$limits$max_result_mb * 1024^2) {
        .ls_stop("Result exceeds this user's max_result_mb download limit.")
      }
      result
    },

    #' @description
    #' Read a worker log for an authenticated-user job.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param id Durable job identifier.
    #' @param stream Standard-output or standard-error stream.
    #' @return A character vector containing log lines.
    logs = function(token, id, stream = c("stdout", "stderr")) {
      auth <- self$authenticate(token, "jobs:read")
      private$queue_for(auth)$logs(id, stream = match.arg(stream))
    },

    #' @description
    #' Cancel an authenticated-user job.
    #' @param token Bearer token issued by [ls_user_create()].
    #' @param id Durable job identifier.
    #' @return `TRUE` when cancellation changed the job state and `FALSE` when
    #'   the job was already terminal, invisibly.
    cancel = function(token, id) {
      auth <- self$authenticate(token, "jobs:write")
      cancelled <- private$queue_for(auth)$cancel(id)
      if (isTRUE(cancelled)) {
        .ls_audit_append(self$root, "job_cancelled", auth$username, list(id = id))
      }
      cancelled
    }
  ),
  private = list(
    queues = NULL,
    queue_for = function(auth) {
      username <- auth$username
      if (!exists(username, envir = private$queues, inherits = FALSE)) {
        workers <- min(self$max_workers_per_user, auth$limits$max_concurrent_jobs)
        assign(username, ls_local_queue(
          self$root, username, workers, limits = auth$limits
        ), envir = private$queues)
      }
      queue <- get(username, envir = private$queues, inherits = FALSE)
      queue$limits <- auth$limits
      queue$max_workers <- min(
        self$max_workers_per_user, auth$limits$max_concurrent_jobs
      )
      queue
    }
  )
)

#' Create the authenticated LibeRties server core
#' @param root Persistent server storage root.
#' @param max_workers_per_user Host-level worker ceiling per tenant.
#' @return A `LibeRServer` object.
#' @examples
#' server <- ls_server(tempfile("liberties-server-"))
#' server$root
#' @export
ls_server <- function(root = .ls_default_root(), max_workers_per_user = 2L) {
  LibeRServer$new(root, max_workers_per_user)
}
