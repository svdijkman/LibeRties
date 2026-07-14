#' Create a versioned LibeR execution job
#'
#' @param type `simulate` or `estimate`.
#' @param model A serializable LibeRation model (never a live external pointer).
#' @param data A serializable NONMEM-style dataset.
#' @param arguments Named arguments passed to the selected LibeRation entry point.
#' @param label Optional human-readable label.
#' @return A serializable `liber_job`.
#' @export
ls_job <- function(type = c("simulate", "estimate"), model, data,
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
    requirements = list(LibeRation = ">= 0.6.0", LibeRtAD = ">= 0.6.0")
  )
}

#' Report queue and remote-worker contract capabilities
#' @export
ls_queue_capabilities <- function() {
  list(
    contract = "liber.job/1",
    job_types = c("simulate", "estimate"),
    states = c("queued", "running", "completed", "failed", "cancelled"),
    worker = "restricted R subprocess with scrubbed environment and C++ LibeRation engine",
    local_platform = R.version$platform,
    remote_target = c("Windows", "Linux", "macOS"),
    integrity = "SHA-256 payload and result digests (MD5 retained for v1 diagnostics)",
    isolation = c("non-executable typed remote contract", "per-tenant filesystem",
                  "wall-time/CPU/RSS enforcement", "single-thread numerical libraries")
  )
}
