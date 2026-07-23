test_that("job contract is serializable and checksummed", {
  job <- ls_job("simulate", model = list(version = 1L),
                data = data.frame(ID = 1, TIME = 0), label = "demo")
  expect_s3_class(job, "liber_job")
  manifest <- ls_job_manifest(job)
  expect_equal(manifest$schema, "liber.job.manifest")
  expect_match(manifest$payload_md5, "^[a-f0-9]{32}$")
  expect_match(manifest$payload_sha256, "^[a-f0-9]{64}$")
  expect_equal(manifest$integrity, "sha256")
  expect_gt(manifest$payload_bytes, 0)
})

test_that("optimal design is an advertised typed job", {
  job <- ls_job("optimal_design", model = list(version = 1L),
                data = list(schema = "liberality.design", version = 1L),
                arguments = list(operation = "evaluate"))
  expect_equal(job$type, "optimal_design")
  expect_true("optimal_design" %in% ls_queue_capabilities()$job_types)
  expect_equal(ls_job_manifest(job)$requirements$LibeRality, ">= 0.2.1")
})

test_that("job ids use 128 bits of cryptographic randomness", {
  ids <- replicate(20, LibeRties:::.ls_new_id())
  expect_true(all(grepl("^[0-9]{8}T[0-9]{6}-[a-f0-9]{32}$", ids)))
  expect_equal(length(unique(ids)), length(ids))
})

test_that("user namespaces reject path traversal", {
  root <- tempfile("queue-")
  expect_error(ls_local_queue(root, user = "../other"), "Invalid user id")
})

test_that("queued jobs can be cancelled without execution", {
  root <- tempfile("queue-")
  queue <- ls_local_queue(root, max_workers = 1)
  job <- ls_job("simulate", model = list(version = 1L),
                data = data.frame(ID = 1, TIME = 0))
  id <- queue$submit(job, start = FALSE)
  expect_equal(queue$status(id)$status, "queued")
  expect_true(queue$cancel(id))
  expect_equal(queue$status(id)$status, "cancelled")
})

test_that("durable queue recovers a dead untracked worker after restart", {
  root <- tempfile("queue-")
  queue <- ls_local_queue(root, max_workers = 1)
  job <- ls_job("simulate", model = list(version = 1L),
                data = data.frame(ID = 1, TIME = 0))
  id <- queue$submit(job, start = FALSE)
  job_dir <- LibeRties:::.ls_job_dir(root, "local", id)
  LibeRties:::.ls_update_meta(job_dir, list(
    status = "running", pid = 2147483647L, pid_started = 0
  ))
  restarted <- ls_local_queue(root, max_workers = 1)
  restarted$poll(start = FALSE)
  expect_equal(restarted$status(id)$status, "failed")
  expect_match(restarted$status(id)$error, "durable-queue recovery")
})

test_that("durable records recover the previous generation after an interrupted write", {
  root <- tempfile("queue-interrupted-")
  queue <- ls_local_queue(root, max_workers = 1)
  job <- ls_job("simulate", model = list(version = 1L),
                data = data.frame(ID = 1, TIME = 0))
  id <- queue$submit(job, start = FALSE)
  job_dir <- LibeRties:::.ls_job_dir(root, "local", id)
  metadata <- LibeRties:::.ls_meta_path(job_dir)
  backup <- paste0(metadata, ".previous")
  expect_true(file.copy(metadata, backup))
  writeBin(charToRaw("interrupted"), metadata)

  expect_warning(
    recovered <- LibeRties:::.ls_read_meta(job_dir),
    "Recovered interrupted durable write"
  )
  expect_equal(recovered$id, id)
  expect_equal(recovered$status, "queued")

  # During an atomic rotation on Windows the primary pathname can briefly be
  # absent while the previous generation is already durable. Public status
  # reads must follow the same recovery path instead of reporting an unknown
  # job.
  unlink(metadata)
  expect_warning(
    recovered_during_rotation <- queue$status(id),
    "Recovered interrupted durable write"
  )
  expect_equal(recovered_during_rotation$id, id)
})
