test_that("LibeRties administration login renders in a real browser", {
  skip_if_not_installed("shinytest2")
  skip_if(Sys.getenv("LIBER_RUN_BROWSER_TESTS") != "true")
  root <- tempfile("liberties-browser-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  driver <- shinytest2::AppDriver$new(
    LibeRties::ls_admin_gui(root, admin_token = "browser-test-admin-token"),
    name = "liberties-browser", width = 1366, height = 768,
    load_timeout = 120000, seed = 20260723
  )
  on.exit(driver$stop(), add = TRUE)
  driver$wait_for_idle()
  expect_identical(driver$get_js("document.title"), "LibeRties")
  expect_match(driver$get_js("document.body.innerText"), "Sign in")
  expect_false(driver$get_js(
    "document.documentElement.scrollWidth > document.documentElement.clientWidth + 2"
  ))
})
