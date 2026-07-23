#' Create a versioned LibeR execution job
#'
#' @param type `simulate`, `estimate`, `estimate_sequence`, `individualise`, `regimen`, or
#'   `optimal_design`.
#' @param model A serializable LibeRation model (never a live external pointer).
#' @param data A serializable NONMEM-style dataset.
#' @param arguments Named arguments passed to the selected LibeRation entry point.
#' @param label Optional human-readable label.
#' @return A serializable `liber_job`.
#' @export
ls_job <- function(type = c("simulate", "estimate", "estimate_sequence", "individualise", "regimen", "optimal_design"), model, data,
                   arguments = list(), label = NULL) {
  type <- match.arg(type)
  if (missing(model) || inherits(model, "NMEngine")) {
    .ls_stop("`model` must be a serializable nm_model, not a compiled pointer-backed engine.")
  }
  if (missing(data)) .ls_stop("`data` is required.")
  if (!is.list(arguments) || is.null(names(arguments)) && length(arguments)) {
    .ls_stop("`arguments` must be a named list.")
  }
  job <- structure(
    list(
      schema = "liber.job",
      version = 1L,
      type = type,
      model = model,
      data = data,
      arguments = arguments,
      label = as.character(label %||% "")[[1L]],
      created = .ls_now()
    ),
    class = "liber_job"
  )
  tryCatch(serialize(job, NULL, version = 3), error = function(e) {
    .ls_stop("Job is not serializable: ", conditionMessage(e))
  })
  job
}

#' Create a typed LibeRary literature job
#'
#' @param type A typed LibeRary pipeline task.
#' @param payload A data-only LibeRary payload.
#' @param arguments Named worker controls, including sanitized provider config.
#' @param label Optional label.
#' @return A serializable `liber_job`.
#' @export
ls_library_job <- function(type = c("library_triage", "library_parse", "library_index",
                                    "library_dual_extract", "library_assess",
                                    "library_adjudicate"), payload,
                           arguments = list(), label = NULL) {
  type <- match.arg(type)
  if (missing(payload) || !is.list(payload)) .ls_stop("`payload` must be a list.")
  if (!is.list(arguments) || (length(arguments) && is.null(names(arguments)))) {
    .ls_stop("`arguments` must be a named list.")
  }
  forbidden <- function(x) {
    if (is.function(x) || is.environment(x) || typeof(x) == "externalptr") return(TRUE)
    if (is.list(x)) return(any(vapply(x, forbidden, logical(1))))
    FALSE
  }
  if (forbidden(payload) || forbidden(arguments)) .ls_stop("Literature jobs may contain data only, not executable or pointer-backed values.")
  job <- structure(list(schema = "liber.job", version = 1L, type = type,
                        model = NULL, data = payload, arguments = arguments,
                        label = as.character(label %||% "")[[1L]], created = .ls_now()),
                   class = "liber_job")
  tryCatch(serialize(job, NULL, version = 3), error = function(e) .ls_stop("Job is not serializable: ", conditionMessage(e)))
  job
}

#' @export
print.liber_job <- function(x, ...) {
  cat("LibeR execution job\n")
  cat("  type:", x$type, " schema:", x$schema, "v", x$version, "\n")
  if (nzchar(x$label)) cat("  label:", x$label, "\n")
  invisible(x)
}

#' Create the transport manifest for a job payload
#'
#' @param job A [ls_job()] object.
#' @return A JSON-compatible manifest with an exact serialized-payload checksum.
#' @export
ls_job_manifest <- function(job) {
  if (!inherits(job, "liber_job")) .ls_stop("`job` must be created by ls_job().")
  raw <- serialize(job, NULL, version = 3)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeBin(raw, tmp)
  list(
    schema = "liber.job.manifest",
    version = 1L,
    job_schema = job$schema,
    job_version = job$version,
    type = job$type,
    created = job$created,
    payload_bytes = length(raw),
    payload_md5 = .ls_md5(tmp),
    payload_sha256 = .ls_sha256(tmp),
    integrity = "sha256",
    requirements = if (startsWith(job$type, "library_")) {
      list(LibeRary = ">= 0.7.3", LibeRties = ">= 0.7.1")
    } else if (identical(job$type, "optimal_design")) {
      list(LibeRality = ">= 0.2.1", LibeRation = ">= 0.8.1", LibeRtAD = ">= 0.7.6")
    } else if (job$type %in% c("individualise", "regimen")) {
      list(LibeRator = ">= 0.2.4", LibeRation = ">= 0.8.1", LibeRtAD = ">= 0.7.6")
    } else list(LibeRation = ">= 0.8.1", LibeRtAD = ">= 0.7.6")
  )
}

#' Report queue and remote-worker contract capabilities
#' @export
ls_queue_capabilities <- function() {
  list(
    contract = "liber.job/1",
    wire_contract = "liber.job.wire/2",
    result_contract = "liber.result.wire/2",
    model_contract = "liberation.model/2",
    job_types = c("simulate", "estimate", "estimate_sequence", "individualise", "regimen", "optimal_design",
                  "library_triage", "library_parse",
                  "library_index", "library_dual_extract", "library_assess",
                  "library_adjudicate"),
    states = c("queued", "running", "completed", "failed", "cancelled"),
    worker = "restricted R subprocess with scrubbed environment, isolated working directory, process-tree accounting, and typed entry points",
    local_platform = R.version$platform,
    remote_target = c("Windows", "Linux", "macOS"),
    integrity = "SHA-256 payload and result digests (MD5 retained for v1 diagnostics)",
    isolation = c("non-executable typed remote contract", "per-tenant filesystem namespace",
                  "process-tree wall-time/CPU/RSS enforcement", "single-thread numerical libraries",
                  "external OS sandbox required for production")
  )
}
