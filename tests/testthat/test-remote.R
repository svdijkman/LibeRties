test_that("HTTP router exposes the typed contract without an RDS upload route", {
  skip_if_not_installed("plumber")
  root <- tempfile("server-")
  server <- ls_server(root)
  router <- ls_api(server)
  expect_s3_class(router, "Plumber")
  printed <- paste(capture.output(print(router)), collapse = "\n")
  expect_match(printed, "/jobs")
  expect_false(grepl("rds", printed, ignore.case = TRUE))
})

test_that("remote client validates its connection settings", {
  expect_error(ls_remote("not-a-url", "token"), "invalid")
  client <- ls_remote("https://example.test/", "token", timeout = 5)
  expect_equal(client$url, "https://example.test")
  expect_equal(client$timeout, 5)
})
