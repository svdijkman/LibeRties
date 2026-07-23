test_that("typed literature jobs round-trip without executable payloads", {
  payload <- list(
    metadata = list(pmid = "123", title = "Population PK"),
    extraction = NULL, full_text = "Evidence", supplement_text = ""
  )
  job <- ls_library_job("library_index", payload,
                        arguments = list(cfg = list(llm = list()), run_assessment = TRUE),
                        label = "Index PMID 123")
  expect_equal(job$type, "library_index")
  decoded <- ls_job_decode(ls_job_encode(job))
  expect_equal(decoded$type, job$type)
  expect_equal(decoded$data$metadata$pmid, "123")
  expect_equal(decoded$data$full_text, "Evidence")
  expect_equal(ls_job_manifest(job)$requirements$LibeRary, ">= 0.7.3")
  expect_error(ls_library_job("library_assess", list(metadata = list(), run = identity)),
               "data only")
})
