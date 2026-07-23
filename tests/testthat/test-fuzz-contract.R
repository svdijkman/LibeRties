test_that("wire decoder rejects random and truncated payloads without escaping R", {
  set.seed(20260723)
  payloads <- c(
    "", "{", "[]", "null", '{"schema":"liber.job.wire/2"',
    replicate(100L, rawToChar(as.raw(sample(32:126, sample(1:80, 1L), TRUE))),
              simplify = TRUE)
  )
  for (payload in payloads) {
    result <- tryCatch(ls_job_decode(payload, max_bytes = 4096L), error = identity)
    expect_true(inherits(result, "error"), info = payload)
  }
})

test_that("wire decoder enforces the byte ceiling before JSON parsing", {
  payload <- paste(rep("x", 1025L), collapse = "")
  expect_error(ls_job_decode(payload, max_bytes = 1024L), "exceeds")
})
