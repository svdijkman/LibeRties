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
  report <- ls_server_preflight(root, "0.0.0.0", behind_tls_proxy = FALSE,
                                policy = policy)
  expect_false(report$ready)
  expect_match(paste(report$issues, collapse = " "), "TLS reverse proxy")
  expect_match(paste(report$issues, collapse = " "), "OS isolation")
  expect_error(ls_server_preflight(root, "0.0.0.0", policy = policy, strict = TRUE),
               "TLS reverse proxy")
})

test_that("resource accounting includes the complete process tree", {
  usage <- LibeRties:::.ls_resource_usage(Sys.getpid())
  expect_true(is.finite(usage$memory_mb) && usage$memory_mb > 0)
  expect_true(is.finite(usage$cpu_seconds) && usage$cpu_seconds >= 0)
  expect_gte(usage$processes, 1L)
})
