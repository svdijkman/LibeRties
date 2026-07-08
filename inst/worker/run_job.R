args <- commandArgs(trailingOnly = TRUE)
job_path <- Sys.getenv("LIBERTIES_JOB_PATH", unset = "")
if (!nzchar(job_path)) {
  job_path <- Sys.getenv("LIBERATION_JOB_PATH", unset = "")
}
if (!nzchar(job_path) && length(args) >= 1L) {
  job_path <- args[[1L]]
}
if (!nzchar(job_path) || !dir.exists(job_path)) {
  stop("Job path not found: ", job_path, call. = FALSE)
}

argv <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", argv[grep("^--file=", argv)])
bootstrap <- if (length(file_arg) == 1L && nzchar(file_arg)) {
  file.path(dirname(normalizePath(file_arg, winslash = "/")), "bootstrap.R")
} else {
  ""
}
if (!nzchar(bootstrap) || !file.exists(bootstrap)) {
  bootstrap <- Sys.getenv("LIBERTIES_WORKER_BOOTSTRAP", unset = "")
}
if (!nzchar(bootstrap) || !file.exists(bootstrap)) {
  stop("Worker bootstrap.R not found.", call. = FALSE)
}
source(bootstrap, local = TRUE)
run_job_worker(job_path)
