test_that("scoped expiring credentials and audit records are enforced", {
  root <- tempfile("liberties-security-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  user <- ls_user_create(
    root, "reader", scopes = "jobs:read",
    expires = Sys.time() + 3600
  )
  server <- ls_server(root)
  expect_identical(server$authenticate(user$token, "jobs:read")$scopes, "jobs:read")
  expect_error(server$authenticate(user$token, "jobs:write"), "scope is insufficient")
  listed <- ls_user_list(root)
  expect_identical(listed$scopes, "jobs:read")
  expect_true(nzchar(listed$expires))
  audit <- ls_audit_read(root)
  expect_true(isTRUE(attr(audit, "valid")))
  expect_true("user_created" %in% audit$action)

  registry <- LibeRties:::.ls_registry_load(root)
  registry$reader$expires <- "2000-01-01T00:00:00Z"
  LibeRties:::.ls_registry_save(root, registry)
  expect_error(server$authenticate(user$token), "expired")
})

test_that("optional storage encryption authenticates all RDS records", {
  root <- tempfile("liberties-encrypted-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  old <- Sys.getenv("LIBERTIES_STORAGE_KEY", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("LIBERTIES_STORAGE_KEY") else
      Sys.setenv(LIBERTIES_STORAGE_KEY = old)
  }, add = TRUE)
  key <- ls_generate_storage_key()
  Sys.setenv(LIBERTIES_STORAGE_KEY = key)
  user <- ls_user_create(root, "encrypted")
  stored <- readRDS(file.path(root, "server", "users.rds"))
  expect_identical(stored$schema, "liberties.encrypted-rds")
  expect_false(grepl(user$token, paste(capture.output(str(stored)), collapse = ""), fixed = TRUE))
  expect_equal(ls_user_list(root)$username, "encrypted")
  report <- ls_server_preflight(
    root, "127.0.0.1", policy = ls_security_policy(require_storage_encryption = TRUE)
  )
  expect_true(report$ready)
  expect_true(report$storage_encrypted)
})

test_that("production preflight distinguishes subprocesses from OS isolation", {
  root <- tempfile("liberties-preflight-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  policy <- ls_security_policy(production = TRUE)
  old_label <- Sys.getenv("LIBERTIES_OS_ISOLATION", unset = NA_character_)
  on.exit(if (is.na(old_label)) Sys.unsetenv("LIBERTIES_OS_ISOLATION") else
    Sys.setenv(LIBERTIES_OS_ISOLATION = old_label), add = TRUE)
  Sys.setenv(LIBERTIES_OS_ISOLATION = "descriptive-label-is-not-proof")
  report <- ls_server_preflight(root, "0.0.0.0", behind_tls_proxy = FALSE,
                                policy = policy)
  expect_false(report$ready)
  expect_match(paste(report$issues, collapse = " "), "TLS reverse proxy")
  expect_match(paste(report$issues, collapse = " "), "OS isolation")
  expect_error(ls_server_preflight(root, "0.0.0.0", policy = policy, strict = TRUE),
               "TLS reverse proxy")

  isolated <- ls_server_preflight(
    root, "127.0.0.1",
    policy = ls_security_policy(
      production = TRUE, require_storage_encryption = FALSE
    ),
    isolation_probe = function() list(
      active = TRUE, provider = "test-sandbox", evidence = "verified by test harness"
    )
  )
  expect_true(isolated$ready)
  expect_true(isolated$os_isolation_active)
  expect_identical(isolated$os_isolation, "test-sandbox")
})

test_that("forwarded addresses are trusted only from configured proxies", {
  untrusted <- ls_security_policy(trusted_proxies = character())
  request <- list(
    REMOTE_ADDR = "203.0.113.8",
    HTTP_X_FORWARDED_FOR = "198.51.100.2"
  )
  expect_identical(LibeRties:::.ls_request_address(request, untrusted), "203.0.113.8")

  trusted <- ls_security_policy(trusted_proxies = c("203.0.113.0/24", "10.0.0.2"))
  request$HTTP_X_FORWARDED_FOR <- "198.51.100.2, 10.0.0.2"
  expect_identical(LibeRties:::.ls_request_address(request, trusted), "198.51.100.2")
})

test_that("rate state is bounded and rotates by minute", {
  state <- new.env(parent = emptyenv())
  first <- LibeRties:::.ls_rate_take(state, "one", 10, 2L, 2L)
  second <- LibeRties:::.ls_rate_take(state, "two", 10, 2L, 2L)
  third <- LibeRties:::.ls_rate_take(state, "three", 10, 2L, 2L)
  fourth <- LibeRties:::.ls_rate_take(state, "four", 10, 2L, 2L)
  expect_true(first$allowed)
  expect_true(second$overflow)
  expect_true(third$allowed)
  expect_false(fourth$allowed)
  expect_lte(length(ls(state, all.names = TRUE)), 2L)
  rotated <- LibeRties:::.ls_rate_take(state, "fresh", 11, 2L, 2L)
  expect_true(rotated$allowed)
  expect_lte(length(ls(state, all.names = TRUE)), 2L)
})

test_that("remote log responses redact secrets and have bounded size", {
  lines <- c(
    "Authorization: Bearer this-is-a-long-secret-token",
    "email=user@example.org", "gradient=0.125", "password=hunter2"
  )
  redacted <- LibeRties:::.ls_redact_logs(lines, max_lines = 3L, max_bytes = 200L)
  expect_false(any(grepl("secret-token|example.org|hunter2", redacted)))
  expect_true(any(grepl("gradient=0.125", redacted, fixed = TRUE)))
  expect_lte(length(redacted), 4L)
})

test_that("terminal logs are authenticated and encrypted when a storage key exists", {
  root <- tempfile("liberties-log-archive-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  old <- Sys.getenv("LIBERTIES_STORAGE_KEY", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("LIBERTIES_STORAGE_KEY") else
    Sys.setenv(LIBERTIES_STORAGE_KEY = old), add = TRUE)
  Sys.setenv(LIBERTIES_STORAGE_KEY = ls_generate_storage_key())
  writeLines(c("iteration 1", "gradient 0.2"), file.path(root, "stdout.log"))
  expect_true(LibeRties:::.ls_seal_job_logs(root))
  expect_false(file.exists(file.path(root, "stdout.log")))
  expect_true(file.exists(file.path(root, "stdout.log.rds")))
  stored <- readRDS(file.path(root, "stdout.log.rds"))
  expect_identical(stored$schema, "liberties.encrypted-rds")
  expect_identical(
    LibeRties:::.ls_read_job_log(root, "stdout"),
    c("iteration 1", "gradient 0.2")
  )
})

test_that("resource accounting includes the complete process tree", {
  usage <- LibeRties:::.ls_resource_usage(Sys.getpid())
  expect_true(is.finite(usage$memory_mb) && usage$memory_mb > 0)
  expect_true(is.finite(usage$cpu_seconds) && usage$cpu_seconds >= 0)
  expect_gte(usage$processes, 1L)
})
