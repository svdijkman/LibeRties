#* LibeRties API
#* @apiTitle LibeRties

#* @filter proxy_trust
function(req, res) {
  secret <- tryCatch(as.character(ls_config()$proxy_shared_secret), error = function(e) "")
  if (length(secret) == 1L && !is.na(secret) && nzchar(secret)) {
    supplied <- LibeRties:::.ls_req_header(req, "X-Proxy-Secret")
    if (!identical(as.character(supplied), secret)) {
      res$status <- 403L
      return(list(error = "Forbidden: request must arrive via the trusted proxy."))
    }
  }
  plumber::forward()
}

#* @filter cors
function(req, res) {
  # CORS only affects browsers; the R client uses curl and is unaffected. Do not
  # emit a wildcard origin - echo only an explicitly configured https origin.
  origin <- tryCatch(as.character(ls_config()$api_cors_origin), error = function(e) "")
  if (length(origin) == 1L && !is.na(origin) && nzchar(origin)) {
    res$setHeader("Access-Control-Allow-Origin", origin)
    res$setHeader("Vary", "Origin")
    res$setHeader("Access-Control-Allow-Headers", "Authorization, Content-Type, X-API-Token, X-Admin-Token")
  }
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200L
    return(list())
  }
  plumber::forward()
}

#* API index (JSON — not a web UI)
#* @get /
#* @serializer json
function() {
  list(
    service = "LibeRties",
    ok = TRUE,
    health = "/v1/health",
    hint = "Use ls_run_admin() for the admin web GUI. API routes are under /v1/."
  )
}

#* Health and version info
#* @get /v1/health
#* @serializer json
function() {
  LibeRties:::.ls_init_storage()
  list(
    ok = TRUE,
    time = LibeRties:::.ls_now(),
    versions = LibeRties:::.ls_version_info()
  )
}

#* Verify API token and return username
#* @get /v1/auth
#* @serializer json
function(req) {
  user <- LibeRties:::.ls_require_user(req)
  list(
    ok = TRUE,
    username = user$username,
    limits = list(
      max_concurrent_jobs = user$limits$max_concurrent_jobs,
      max_disk_mb = user$limits$max_disk_mb,
      max_cpu = user$limits$max_cpu,
      max_memory_mb = user$limits$max_memory_mb
    )
  )
}

#* List datasets available on this cluster
#* @get /v1/datasets
#* @serializer json
function(req) {
  user <- LibeRties:::.ls_require_user(req)
  ds <- LibeRties:::.ls_dataset_list(username = user$username)
  list(datasets = ds)
}

#* Submit a job
#* @post /v1/jobs
#* @serializer json
function(req) {
  user <- LibeRties:::.ls_require_user(req)
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
    error = function(e) stop("Invalid JSON body.", call. = FALSE)
  )
  meta <- LibeRties:::.ls_job_submit(user$username, user$limits, body)
  list(job = meta)
}

#* List jobs for authenticated user
#* @get /v1/jobs
#* @serializer json
function(req) {
  user <- LibeRties:::.ls_require_user(req)
  df <- LibeRties:::.ls_job_list(user$username, reconcile = "active")
  list(jobs = df)
}

#* Job status
#* @get /v1/jobs/<job_id>
#* @serializer json
function(job_id, req, res) {
  user <- LibeRties:::.ls_require_user(req)
  meta <- LibeRties:::.ls_job_read_meta(user$username, job_id)
  if (is.null(meta)) {
    res$status <- 404L
    return(list(error = "Job not found."))
  }
  job_path <- LibeRties:::.ls_job_path(user$username, job_id)
  if (identical(meta$status, "running") &&
      identical(LibeRties:::.ls_job_process_alive(job_path), TRUE)) {
    # Trust callr liveness; avoid reconcile side-effects during long inner optim.
  } else {
    meta <- LibeRties:::.ls_job_status(user$username, job_id)
    if (is.null(meta)) {
      res$status <- 404L
      return(list(error = "Job not found."))
    }
  }
  vers <- LibeRties:::.ls_version_info()
  client_lr <- req$HTTP_X_LIBERATION_VERSION %||% ""
  client_ad <- req$HTTP_X_LIBERTAD_VERSION %||% ""
  warnings <- character()
  if (nzchar(client_lr) && nzchar(vers$LibeRation) &&
      client_lr != vers$LibeRation) {
    warnings <- c(warnings, paste0(
      "LibeRation version mismatch: client ", client_lr,
      ", server ", vers$LibeRation
    ))
  }
  if (nzchar(client_ad) && nzchar(vers$LibeRtAD) &&
      client_ad != vers$LibeRtAD) {
    warnings <- c(warnings, paste0(
      "LibeRtAD version mismatch: client ", client_ad,
      ", server ", vers$LibeRtAD
    ))
  }
  list(job = meta, warnings = warnings)
}

#* Worker log tail
#* @get /v1/jobs/<job_id>/log
#* @serializer json
function(job_id, req, res, tail = 100) {
  user <- LibeRties:::.ls_require_user(req)
  meta <- LibeRties:::.ls_job_read_meta(user$username, job_id)
  if (is.null(meta)) {
    res$status <- 404L
    return(list(error = "Job not found."))
  }
  list(log = LibeRties:::.ls_job_log(user$username, job_id, tail = as.integer(tail)))
}

#* Job result (base64-encoded result.rds)
#* @get /v1/jobs/<job_id>/result
#* @serializer json
function(job_id, req, res) {
  user <- LibeRties:::.ls_require_user(req)
  meta <- LibeRties:::.ls_job_status(user$username, job_id)
  if (is.null(meta)) {
    res$status <- 404L
    return(list(error = "Job not found."))
  }
  if (!identical(meta$status, "success")) {
    res$status <- 409L
    return(list(error = paste("Job not successful:", meta$status)))
  }
  list(result_b64 = LibeRties:::.ls_job_result_b64(user$username, job_id))
}

#* Cancel a job
#* @delete /v1/jobs/<job_id>
#* @serializer json
function(job_id, req) {
  user <- LibeRties:::.ls_require_user(req)
  meta <- LibeRties:::.ls_job_cancel(user$username, job_id)
  list(job = meta)
}

#* Remove finished jobs for authenticated user
#* @post /v1/jobs/cleanup
#* @serializer json
function(req) {
  user <- LibeRties:::.ls_require_user(req)
  n <- LibeRties:::.ls_job_cleanup(user$username)
  list(removed = n)
}

#* --- Admin routes ---

#* List all users (admin)
#* @get /v1/admin/users
#* @serializer json
function(req) {
  LibeRties:::.ls_require_admin(req)
  list(users = LibeRties::ls_user_list())
}

#* Create user (admin)
#* @post /v1/admin/users
#* @serializer json
function(req) {
  LibeRties:::.ls_require_admin(req)
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  out <- LibeRties::ls_user_create(
    body$username,
    limits = as.list(body$limits %||% list()),
    enabled = isTRUE(body$enabled %||% TRUE)
  )
  list(user = out$username, token = out$token, limits = out$limits)
}

#* Update user limits (admin)
#* @put /v1/admin/users/<username>/limits
#* @serializer json
function(username, req) {
  LibeRties:::.ls_require_admin(req)
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  LibeRties::ls_user_set_limits(
    username,
    max_concurrent_jobs = body$max_concurrent_jobs,
    max_disk_mb = body$max_disk_mb,
    max_cpu = body$max_cpu,
    max_memory_mb = body$max_memory_mb,
    enabled = body$enabled
  )
  list(ok = TRUE, user = username)
}

#* Issue new token (admin)
#* @post /v1/admin/users/<username>/token
#* @serializer json
function(username, req) {
  LibeRties:::.ls_require_admin(req)
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = TRUE),
                   error = function(e) list())
  token <- LibeRties::ls_user_issue_token(
    username,
    current_token = body$current_token,
    force = isTRUE(body$force %||% FALSE)
  )
  list(token = token)
}

#* Remove user (admin)
#* @delete /v1/admin/users/<username>
#* @serializer json
function(username, req, remove_sandbox = FALSE) {
  LibeRties:::.ls_require_admin(req)
  LibeRties::ls_user_remove(username, remove_sandbox = isTRUE(remove_sandbox))
  list(ok = TRUE)
}

#* List all jobs (admin)
#* @get /v1/admin/jobs
#* @serializer json
function(req) {
  LibeRties:::.ls_require_admin(req)
  list(jobs = LibeRties:::.ls_job_list_all())
}

#* Remove all finished jobs (admin)
#* @post /v1/admin/jobs/cleanup
#* @serializer json
function(req) {
  LibeRties:::.ls_require_admin(req)
  n <- LibeRties:::.ls_job_cleanup_all()
  list(removed = n)
}

#* Register dataset (admin)
#* @post /v1/admin/datasets
#* @serializer json
function(req) {
  LibeRties:::.ls_require_admin(req)
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  entry <- LibeRties:::.ls_dataset_register(
    body$dataset_id,
    body$file_path,
    label = body$label,
    owner = body$owner,
    allowed_users = body$allowed_users,
    public = isTRUE(body$public %||% FALSE)
  )
  list(dataset = entry)
}

#* List datasets (admin)
#* @get /v1/admin/datasets
#* @serializer json
function(req) {
  LibeRties:::.ls_require_admin(req)
  list(datasets = LibeRties:::.ls_dataset_list())
}
