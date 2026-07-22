test_that("local worker reconstructs and runs a LibeRation C++ engine", {
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT"), ADVAN = 1,
    DOSECMP = 1, OBSCMP = 1,
    PRED = "CL=THETA(1)\nV=THETA(2)\nS1=V", ERROR = "Y=F",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20))
  )
  job <- ls_job(
    "simulate", model,
    data.frame(ID = 1, TIME = c(0, 1), EVID = c(1, 0), AMT = c(100, 0)),
    label = "ADVAN1 smoke test"
  )
  queue <- ls_local_queue(tempfile("queue-"), max_workers = 1)
  id <- queue$submit(job)
  status <- queue$wait(id, timeout = 30, poll_interval = 0.05)
  expect_equal(status$status, "completed", info = status$error)
  result <- queue$result(id)
  expect_equal(result$IPRED, c(5, 5 * exp(-0.1)), tolerance = 1e-10)
  expect_equal(status$isolation, "restricted-subprocess")
  expect_true(is.finite(status$peak_memory_mb))
  expect_equal(status$termination_reason, "completed normally")
})

test_that("estimation gradients are retained in the worker log", {
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "DV", "MDV"),
    ADVAN = 1, DOSECMP = 1, OBSCMP = 1,
    PRED = "CL=THETA(1)*exp(ETA(1)); V=THETA(2); S1=V",
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20), FIX = c(FALSE, TRUE)),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.09, FIX = TRUE),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.2, FIX = TRUE)
  )
  times <- c(0, 1, 4, 12)
  data <- do.call(rbind, lapply(seq_len(3L), function(id) {
    prediction <- 5 * exp(-0.1 * times)
    data.frame(
      ID = id, TIME = times, EVID = c(1, 0, 0, 0),
      AMT = c(100, 0, 0, 0), MDV = c(1, 0, 0, 0),
      DV = c(NA, prediction[-1] + c(0.05, -0.03, 0.02))
    )
  }))
  queue <- ls_local_queue(tempfile("gradient-queue-"), max_workers = 1)
  id <- queue$submit(ls_job(
    "estimate", model, data,
    arguments = list(method = "FOCEI", maxit = 2L, print_every = 25L)
  ))
  status <- queue$wait(id, timeout = 60, poll_interval = 0.05)
  expect_equal(status$status, "completed", info = status$error)
  deadline <- Sys.time() + 2
  worker_log <- character()
  while (!length(worker_log) && Sys.time() < deadline) {
    Sys.sleep(0.05)
    worker_log <- queue$logs(id, stream = "stdout")
  }
  expect_match(worker_log, "SCALED GRADIENT")
})

test_that("queue monitor terminates workers that exceed resource policy", {
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT"), ADVAN = 1,
    PRED = "CL=THETA(1); V=THETA(2); S1=V", ERROR = "Y=F",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20))
  )
  job <- ls_job(
    "simulate", model,
    data.frame(ID = 1, TIME = c(0, 1), EVID = c(1, 0), AMT = c(100, 0)),
    arguments = list(nsim = 10000L)
  )
  queue <- ls_local_queue(
    tempfile("limited-"), limits = list(max_memory_mb = 1)
  )
  id <- queue$submit(job)
  status <- queue$wait(id, timeout = 30, poll_interval = 0.02)
  expect_equal(status$status, "failed")
  expect_match(status$error, "Resource limit exceeded: memory limit")
  expect_match(status$termination_reason, "memory limit")
})
