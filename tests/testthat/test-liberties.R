library(LibeRties)

test_that("user limits are enforced", {
  root <- tempfile("ls_test_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)

  u <- ls_user_create(
    "alice",
    limits = list(max_concurrent_jobs = 1L, max_disk_mb = 100L)
  )
  expect_equal(u$username, "alice")
  expect_true(grepl("^lr_", u$token))

  auth <- LibeRties:::.ls_user_from_token(u$token)
  expect_equal(auth$username, "alice")

  ls_user_set_limits("alice", max_concurrent_jobs = 0L)
  users <- LibeRties:::.ls_users_load()
  limits <- users[["alice"]]
  expect_equal(LibeRties:::.ls_user_running_jobs("alice"), 0L)
  expect_no_error(LibeRties:::.ls_check_user_limits("alice", limits))
})

test_that("dataset MD5 mismatch fails", {
  root <- tempfile("ls_ds_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)
  LibeRties:::.ls_init_storage()

  tmp <- tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1:3), tmp)
  LibeRties:::.ls_dataset_register("demo", tmp)

  expect_error(
    LibeRties:::.ls_dataset_resolve("demo", "badmd5"),
    "MD5 mismatch"
  )
})

test_that("admin token verification works", {
  root <- tempfile("ls_adm_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)
  ls_admin_token_set("secret-admin")
  expect_true(LibeRties:::.ls_admin_token_verify("secret-admin"))
  expect_false(LibeRties:::.ls_admin_token_verify("wrong"))
})

test_that("token auth round trip", {
  root <- tempfile("ls_auth_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)

  u <- ls_user_create("alice")
  expect_equal(LibeRties:::.ls_user_from_token(u$token)$username, "alice")
  expect_equal(
    LibeRties:::.ls_user_from_token(paste0("  ", u$token, "  "))$username,
    "alice"
  )
  expect_null(LibeRties:::.ls_user_from_token("not-a-real-token"))
  ok <- ls_auth_test_token(u$token)
  expect_true(ok$ok)
  expect_equal(ok$username, "alice")
})

test_that("ls_config loads custom sandbox from default config file", {
  root <- tempfile("ls_cfg_")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  default_root <- LibeRties:::.ls_default_sandbox_root()
  cfg_path <- file.path(default_root, "config.json")
  old_cfg <- ls_config()
  old_file <- if (file.exists(cfg_path)) readLines(cfg_path, warn = FALSE) else NULL
  withr::defer({
    ls_config_reset()
    if (is.null(old_file)) {
      unlink(cfg_path)
    } else {
      writeLines(old_file, cfg_path)
    }
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_reset()
  dir.create(default_root, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(sandbox_root = root), cfg_path, auto_unbox = TRUE)
  ls_config_reset()
  cfg <- ls_config()
  expect_equal(cfg$sandbox_root, root)
  expect_equal(LibeRties:::.ls_resolve_sandbox_root(), root)
})

test_that("bearer token is read from X-API-Token header", {
  req <- list(HTTP_X_API_TOKEN = "lr_test_token")
  expect_equal(LibeRties:::.ls_bearer_token(req), "lr_test_token")
})

test_that("version info is JSON-serializable", {
  vers <- LibeRties:::.ls_version_info()
  expect_type(vers$LibeRties, "character")
  expect_type(vers$LibeRation, "character")
  expect_type(vers$LibeRtAD, "character")
  expect_silent(jsonlite::toJSON(vers, auto_unbox = TRUE))
})

test_that("base64 RDS round-trip handles gzip compression", {
  skip_if_not_installed("LibeRation")
  sim <- LibeRation::nm_synthetic_theo(n_sub = 2L)
  b64 <- LibeRation:::.nm_rds_b64(sim$model)
  js <- jsonlite::toJSON(list(model_b64 = b64), auto_unbox = TRUE)
  body <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  model <- LibeRties:::.ls_rds_from_raw(jsonlite::base64_dec(body$model_b64))
  expect_s3_class(model, "nm_model")
})

test_that("sibling package roots resolve for worker env", {
  skip_if_not_installed("LibeRation")
  lr <- LibeRties:::.ls_sibling_pkg_root("LibeRation")
  expect_true(!nzchar(lr) || dir.exists(lr))
  env <- LibeRties:::.ls_job_worker_env()
  expect_true(env$mode %in% c("dev", "installed"))
  expect_true(nzchar(env$nm_root) || identical(env$mode, "installed"))
})

test_that("est args normalize to stable cpp engines on server", {
  skip_if_not_installed("LibeRation")
  sim <- LibeRation::nm_synthetic_theo(n_sub = 2L, seed = 1L)
  norm <- LibeRties:::.ls_job_normalize_est_args(
    list(
      grad = "auto",
      pk_engine = "auto",
      control = list(maxit = 20, compute_inference = FALSE)
    ),
    "FO",
    sim$model
  )
  expect_equal(norm$grad, "cpp")
  expect_equal(norm$pk_engine, "cpp")
  norm_lap <- LibeRties:::.ls_job_normalize_est_args(
    list(grad = "auto", pk_engine = "auto", n_quad = 3),
    "LAPLACE",
    sim$model
  )
  expect_equal(norm_lap$engine, "cpp")
  expect_equal(norm_lap$grad, "cpp")
  norm_focei <- LibeRties:::.ls_job_normalize_est_args(
    list(grad = "cpp", pk_engine = "auto"),
    "FOCEI",
    sim$model
  )
  expect_equal(norm_focei$pk_engine, "cpp")
  expect_equal(norm_focei$grad, "auto")
})

test_that("reconcile marks dead workers as error", {
  root <- tempfile("ls_rec_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)
  LibeRties:::.ls_init_storage()
  u <- ls_user_create("alice")
  job_id <- "job_test_dead"
  job_path <- LibeRties:::.ls_job_path("alice", job_id)
  dir.create(job_path, recursive = TRUE, showWarnings = FALSE)
  meta <- list(
    id = job_id,
    user = "alice",
    status = "running",
    job_type = "est",
    pid = 99999999L,
    started = format(Sys.time() - 7200, "%Y-%m-%d %H:%M:%S"),
    error = ""
  )
  saveRDS(meta, file.path(job_path, "meta.rds"))
  st <- LibeRties:::.ls_job_status("alice", job_id)
  expect_equal(st$status, "error")
  expect_match(st$error, "Worker exited")
})

test_that("jobs stay queued when concurrent limit is reached", {
  root <- tempfile("ls_queue_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)
  LibeRties:::.ls_init_storage()
  u <- ls_user_create("alice", limits = list(max_concurrent_jobs = 1L))
  limits <- LibeRties:::.ls_user_limits("alice")

  running_id <- "job_running"
  running_path <- LibeRties:::.ls_job_path("alice", running_id)
  dir.create(running_path, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(
    id = running_id,
    user = "alice",
    status = "running",
    job_type = "est",
    pid = 99999999L,
    started = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    error = ""
  ), file.path(running_path, "meta.rds"))
  saveRDS(list(model = list(), data = data.frame()), file.path(running_path, "args.rds"))

  queued_id <- "job_queued"
  queued_path <- LibeRties:::.ls_job_path("alice", queued_id)
  dir.create(queued_path, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(
    id = queued_id,
    user = "alice",
    status = "queued",
    job_type = "est",
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    started = "",
    error = ""
  ), file.path(queued_path, "meta.rds"))
  saveRDS(list(model = list(), data = data.frame()), file.path(queued_path, "args.rds"))

  st <- LibeRties:::.ls_job_status("alice", queued_id)
  expect_equal(st$status, "queued")
  expect_equal(LibeRties:::.ls_user_queued_jobs("alice"), 1L)
  expect_equal(LibeRties:::.ls_user_running_jobs("alice"), 1L)
  launched <- LibeRties:::.ls_job_try_launch("alice", queued_id, limits)
  expect_equal(launched$status, "queued")
})

test_that("finished jobs can be cleaned up", {
  root <- tempfile("ls_cleanup_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)
  LibeRties:::.ls_init_storage()
  ls_user_create("alice")
  for (st in c("success", "running")) {
    job_id <- paste0("job_", st)
    job_path <- LibeRties:::.ls_job_path("alice", job_id)
    dir.create(job_path, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(
      id = job_id,
      user = "alice",
      status = st,
      job_type = "est",
      created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      error = ""
    ), file.path(job_path, "meta.rds"))
  }
  n <- LibeRties:::.ls_job_cleanup("alice")
  expect_equal(n, 1L)
  expect_true(dir.exists(LibeRties:::.ls_job_path("alice", "job_running")))
  expect_false(dir.exists(LibeRties:::.ls_job_path("alice", "job_success")))
})
