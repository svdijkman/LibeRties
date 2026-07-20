test_that("all LibeRary pipeline jobs round-trip through the safe wire contract", {
  metadata <- list(pmid = "1", title = "Test", abstract = "Population PK")
  types <- c("library_triage", "library_parse", "library_index",
             "library_dual_extract", "library_assess", "library_adjudicate")
  for (type in types) {
    job <- ls_library_job(type, list(metadata = metadata), arguments = list(test = TRUE))
    rebuilt <- ls_job_decode(ls_job_encode(job))
    expect_equal(rebuilt$type, type)
    expect_equal(rebuilt$data$metadata$pmid, "1")
  }
  expect_true(all(types %in% ls_queue_capabilities()$job_types))
})
