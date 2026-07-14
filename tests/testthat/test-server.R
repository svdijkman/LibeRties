test_that("server tokens are high entropy and only hashes are persisted", {
  root <- tempfile("server-")
  user <- ls_user_create(root, "alice")
  expect_match(user$token, "^lr_[0-9]{8}_[a-f0-9]{64}$")
  registry <- readRDS(file.path(root, "server", "users.rds"))
  expect_null(registry$alice$token)
  expect_match(registry$alice$token_hash, "^[a-f0-9]{64}$")
  expect_false(grepl(user$token, paste(capture.output(str(registry)), collapse = ""), fixed = TRUE))
  expect_equal(ls_server(root)$authenticate(user$token)$username, "alice")
})

test_that("token rotation revokes the old credential", {
  root <- tempfile("server-")
  user <- ls_user_create(root, "alice")
  server <- ls_server(root)
  token <- ls_user_rotate_token(root, "alice")
  expect_error(server$authenticate(user$token), "invalid API token")
  expect_equal(server$authenticate(token)$username, "alice")
})

test_that("authenticated operations cannot cross tenant namespaces", {
  root <- tempfile("server-")
  alice <- ls_user_create(root, "alice")
  bob <- ls_user_create(root, "bob")
  server <- ls_server(root, max_workers_per_user = 1)
  job <- ls_job("simulate", model = list(version = 1L),
                data = data.frame(ID = 1, TIME = 0))
  id <- server$submit(alice$token, job, start = FALSE)
  expect_equal(server$list(alice$token)$id, id)
  expect_equal(nrow(server$list(bob$token)), 0L)
  expect_error(server$status(bob$token, id), "Unknown job id")
})

test_that("disabled users and quota violations are rejected", {
  root <- tempfile("server-")
  user <- ls_user_create(root, "alice", limits = list(
    max_queued_jobs = 1L, max_payload_mb = 0.0001
  ))
  server <- ls_server(root)
  job <- ls_job("simulate", model = list(version = 1L),
                data = data.frame(ID = 1, TIME = 0))
  expect_error(server$submit(user$token, job, start = FALSE), "max_payload_mb")
  ls_user_update(root, "alice", enabled = FALSE)
  expect_error(server$authenticate(user$token), "disabled")
})

test_that("server accounts expose complete execution resource limits", {
  root <- tempfile("server-")
  user <- ls_user_create(root, "alice", limits = list(
    max_runtime_seconds = 120, max_cpu_seconds = 90, max_memory_mb = 512
  ))
  authenticated <- ls_server(root)$authenticate(user$token)
  expect_equal(authenticated$limits$max_runtime_seconds, 120)
  expect_equal(authenticated$limits$max_cpu_seconds, 90)
  expect_equal(authenticated$limits$max_memory_mb, 512)
  listed <- ls_user_list(root)
  expect_equal(listed$max_memory_mb, 512)
})

test_that("server user names persist and can be updated without changing authentication", {
  root <- tempfile("server-names-")
  user <- ls_user_create(
    root, "alice", first_name = "Alice", last_name = "Example"
  )
  listed <- ls_user_list(root)
  expect_equal(listed$first_name, "Alice")
  expect_equal(listed$last_name, "Example")
  ls_user_update(root, "alice", first_name = "Alicia", last_name = "Researcher")
  updated <- ls_user_list(root)
  expect_equal(updated$first_name, "Alicia")
  expect_equal(updated$last_name, "Researcher")
  expect_equal(ls_server(root)$authenticate(user$token)$username, "alice")
})
