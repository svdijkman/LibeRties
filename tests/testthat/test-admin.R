test_that("server administration app uses an independent protected login", {
  root <- tempfile("admin-")
  app <- ls_admin_gui(root, admin_token = "a-strong-admin-token")
  expect_s3_class(app, "shiny.appobj")
  expect_error(ls_admin_gui(root, admin_token = "short"), "at least 16")
  expect_error(
    ls_run_admin(root, "a-strong-admin-token", host = "0.0.0.0", launch.browser = NULL),
    "loopback"
  )
  favicon_path <- system.file("admin-assets", "favicon.svg", package = "LibeRties")
  expect_true(file.exists(favicon_path))
  favicon <- paste(readLines(favicon_path, warn = FALSE), collapse = "\n")
  expect_match(favicon, 'width="1000"', fixed = TRUE)
  expect_match(favicon, 'id="liberties-red"', fixed = TRUE)
  expect_match(favicon, 'href="data:image/png;base64,', fixed = TRUE)
  expect_false(grepl("<filter", favicon, fixed = TRUE))
  expect_false(grepl('<circle cx="16"', favicon, fixed = TRUE))

  expect_no_error(shiny::testServer(app[["serverFuncSource"]](), {
    session$flushReact()
    gate <- paste(as.character(output$gate), collapse = "\n")
    expect_match(gate, "LibeRties administration", fixed = TRUE)
    expect_match(paste(as.character(output$notice), collapse = "\n"),
                 "la-message-bar", fixed = TRUE)
  }))
})

test_that("admin visual controls match the client theme and message patterns", {
  root <- tempfile("admin-theme-")
  app <- ls_admin_gui(root, admin_token = "a-strong-admin-token")
  css <- paste(readLines(
    system.file("admin-assets", "admin.css", package = "LibeRties"),
    warn = FALSE
  ), collapse = "\n")
  expect_match(css, ".la-theme-toggle", fixed = TRUE)
  expect_match(css, ".la-message-bar", fixed = TRUE)
  expect_match(css, ".table.table-striped", fixed = TRUE)
  expect_match(css, "background:var(--la-soft)", fixed = TRUE)
  shiny::testServer(app[["serverFuncSource"]](), {
    session$setInputs(admin_token = "a-strong-admin-token", login = 1)
    session$flushReact()
    gate <- paste(as.character(output$gate), collapse = "\n")
    expect_match(gate, "la-theme-toggle", fixed = TRUE)
    expect_match(gate, "la-theme-checkbox", fixed = TRUE)
    notice <- paste(as.character(output$notice), collapse = "\n")
    expect_match(notice, "la-message-dot", fixed = TRUE)
    expect_match(notice, "Administrator session authenticated", fixed = TRUE)
  })
})

test_that("admin login and create-user controls drive the server registry", {
  root <- tempfile("admin-workflow-")
  app <- ls_admin_gui(root, admin_token = "a-strong-admin-token")
  shiny::testServer(app[["serverFuncSource"]](), {
    session$setInputs(admin_token = "a-strong-admin-token", login = 1)
    session$flushReact()
    expect_true(any(grepl("Server administration", as.character(output$gate), fixed = TRUE)))
    session$setInputs(
      new_username = "alice", new_enabled = TRUE,
      new_first_name = "Alice", new_last_name = "Example",
      new_workers = 2, new_queued = 20, new_payload = 100, new_result = 500,
      new_storage = 5120, new_runtime = 86400, new_cpu = 86400, new_memory = 4096,
      create_user = 1
    )
    session$flushReact()
  })
  listed <- ls_user_list(root)
  expect_equal(listed$username, "alice")
  expect_equal(listed$first_name, "Alice")
  expect_equal(listed$last_name, "Example")
})

test_that("user deletion is explicit and refuses active or retained jobs", {
  root <- tempfile("admin-delete-")
  user <- ls_user_create(root, "alice")
  queue <- ls_local_queue(root, "alice")
  job <- ls_job("simulate", model = list(version = 1L), data = data.frame(ID = 1, TIME = 0))
  id <- queue$submit(job, start = FALSE)
  expect_error(ls_user_delete(root, "alice", remove_jobs = TRUE), "active jobs")
  expect_true(queue$cancel(id))
  expect_true(ls_user_delete(root, "alice", remove_jobs = TRUE))
  expect_equal(nrow(ls_user_list(root)), 0L)
  expect_error(ls_server(root)$authenticate(user$token), "invalid API token")
})

test_that("admin restart reloads users and retained jobs from the same root", {
  root <- tempfile("admin-restart-")
  ls_user_create(root, "persistent-user")
  queue <- ls_local_queue(root, "persistent-user")
  id <- queue$submit(ls_job(
    "simulate", model = list(version = 1L), data = data.frame(ID = 1, TIME = 0),
    label = "retained job"
  ), start = FALSE)
  queue$cancel(id)

  app <- ls_admin_gui(root, admin_token = "a-strong-admin-token")
  shiny::testServer(app[["serverFuncSource"]](), {
    session$setInputs(admin_token = "a-strong-admin-token", login = 1)
    session$flushReact()
    session$setInputs(admin_section = "Jobs", refresh_jobs = 1)
    session$flushReact()
    expect_match(paste(as.character(output$users_list), collapse = "\n"),
                 "persistent-user", fixed = TRUE)
    expect_match(paste(as.character(output$jobs_list), collapse = "\n"),
                 "retained job", fixed = TRUE)
  })
})

test_that("admin user search and row selection include human names", {
  root <- tempfile("admin-search-")
  ls_user_create(root, "scientist-1", first_name = "Ada", last_name = "Lovelace")
  ls_user_create(root, "scientist-2", first_name = "Grace", last_name = "Hopper")
  app <- ls_admin_gui(root, admin_token = "a-strong-admin-token")
  shiny::testServer(app[["serverFuncSource"]](), {
    session$setInputs(admin_token = "a-strong-admin-token", login = 1)
    session$flushReact()
    session$setInputs(user_search = "Lovelace")
    session$flushReact()
    rendered <- paste(as.character(output$users_list), collapse = "\n")
    expect_match(rendered, "scientist-1", fixed = TRUE)
    expect_false(grepl("scientist-2", rendered, fixed = TRUE))
    session$setInputs(admin_user_pick = "scientist-1")
    session$flushReact()
    expect_match(paste(as.character(output$selected_user_label), collapse = "\n"),
                 "scientist-1", fixed = TRUE)
  })
})

test_that("server metrics split jobs by state on the Server tab", {
  root <- tempfile("admin-metrics-")
  ls_user_create(root, "metrics-user")
  queue <- ls_local_queue(root, "metrics-user")
  id <- queue$submit(ls_job(
    "simulate", model = list(version = 1L), data = data.frame(ID = 1, TIME = 0)
  ), start = FALSE)
  queue$cancel(id)
  app <- ls_admin_gui(root, admin_token = "a-strong-admin-token")
  shiny::testServer(app[["serverFuncSource"]](), {
    session$setInputs(admin_token = "a-strong-admin-token", login = 1,
                      admin_section = "Server")
    session$flushReact()
    rendered <- paste(as.character(output$runtime), collapse = "\n")
    expect_match(rendered, "Running jobs", fixed = TRUE)
    expect_match(rendered, "Failed jobs", fixed = TRUE)
    expect_match(rendered, "Cancelled jobs", fixed = TRUE)
    expect_match(rendered, "Completed jobs", fixed = TRUE)
  })
})

test_that("LIBERTIES_ROOT provides a shared persistent server root", {
  old <- Sys.getenv("LIBERTIES_ROOT", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("LIBERTIES_ROOT") else
    Sys.setenv(LIBERTIES_ROOT = old), add = TRUE)
  root <- tempfile("configured-root-")
  Sys.setenv(LIBERTIES_ROOT = root)
  expect_equal(normalizePath(LibeRties:::.ls_default_root(), mustWork = FALSE),
               normalizePath(root, mustWork = FALSE))
})
