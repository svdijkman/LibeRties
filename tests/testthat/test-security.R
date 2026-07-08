library(LibeRties)

test_that("token entropy is 256-bit", {
  tok <- LibeRties:::.ls_generate_token()
  expect_true(grepl("^lr_[0-9]{8}_[0-9a-f]{64}$", tok))
  # Two tokens must differ.
  expect_false(identical(tok, LibeRties:::.ls_generate_token()))
})

test_that("AEAD round-trip and tamper/wrong-key detection", {
  skip_if_not_installed("sodium")
  key <- LibeRties:::.ls_dek_generate()
  obj <- list(theta = c(1.2, 3.4), patient = "sensitive-id")
  env <- LibeRties:::.ls_aead_encrypt_obj(obj, key)
  expect_identical(LibeRties:::.ls_aead_decrypt_obj(env, key), obj)

  # Wrong key must fail.
  wrong <- LibeRties:::.ls_dek_generate()
  expect_error(LibeRties:::.ls_aead_decrypt_obj(env, wrong))

  # Tampered ciphertext must fail (authenticated encryption).
  bad <- env
  bad$ct[1L] <- as.raw(bitwXor(as.integer(bad$ct[1L]), 1L))
  expect_error(LibeRties:::.ls_aead_decrypt_obj(bad, key))
})

test_that("DEK wrap/unwrap under user key", {
  skip_if_not_installed("sodium")
  salt <- LibeRties:::.ls_new_salt_hex()
  uk <- LibeRties:::.ls_derive_uk("lr_20260101_" , salt)
  dek <- LibeRties:::.ls_dek_generate()
  wrapped <- LibeRties:::.ls_dek_wrap(dek, uk)
  expect_identical(LibeRties:::.ls_dek_unwrap(wrapped, uk), dek)

  uk2 <- LibeRties:::.ls_derive_uk("different-token", salt)
  expect_error(LibeRties:::.ls_dek_unwrap(wrapped, uk2))
})

test_that("user key derivation is deterministic per (token, salt)", {
  skip_if_not_installed("sodium")
  salt <- LibeRties:::.ls_new_salt_hex()
  a <- LibeRties:::.ls_derive_uk("tok-abc", salt)
  b <- LibeRties:::.ls_derive_uk("tok-abc", salt)
  expect_identical(a, b)
  # Different salt => different key.
  c <- LibeRties:::.ls_derive_uk("tok-abc", LibeRties:::.ls_new_salt_hex())
  expect_false(identical(a, c))
})

test_that("encrypted file round-trip persists ciphertext only", {
  skip_if_not_installed("sodium")
  key <- LibeRties:::.ls_dek_generate()
  obj <- list(model = "M", data = data.frame(dv = c(1.1, 2.2)))
  path <- tempfile(fileext = ".enc")
  withr::defer(unlink(path))
  LibeRties:::.ls_encrypt_to_file(obj, key, path)
  # Envelope on disk has no plaintext fields.
  raw_env <- readRDS(path)
  expect_null(raw_env$model)
  expect_true(is.raw(raw_env$ct) && is.raw(raw_env$nonce))
  expect_identical(LibeRties:::.ls_decrypt_from_file(key, path), obj)
})

test_that("dataset access is scoped per tenant", {
  skip_if_not_installed("sodium")
  root <- tempfile("ls_ds_scope_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)
  LibeRties:::.ls_init_storage()
  ls_user_create("alice"); ls_user_create("bob")

  tmp <- tempfile(fileext = ".rds"); saveRDS(data.frame(x = 1:3), tmp)
  LibeRties:::.ls_dataset_register("shared", tmp, public = TRUE)
  LibeRties:::.ls_dataset_register("alice_only", tmp, owner = "alice")
  LibeRties:::.ls_dataset_register("legacy_no_owner", tmp)

  # Cross-tenant listing is filtered.
  expect_false("alice_only" %in% LibeRties:::.ls_dataset_list("bob")$id)
  expect_true("alice_only" %in% LibeRties:::.ls_dataset_list("alice")$id)
  expect_true("shared" %in% LibeRties:::.ls_dataset_list("bob")$id)

  # Cross-tenant resolve is denied (and does not reveal existence).
  expect_error(LibeRties:::.ls_dataset_resolve("alice_only", username = "bob"),
               "Unknown dataset")
  expect_silent(LibeRties:::.ls_dataset_resolve("alice_only", username = "alice"))
  expect_false("legacy_no_owner" %in% LibeRties:::.ls_dataset_list("bob")$id)
})

test_that("job_id path traversal is rejected", {
  expect_error(LibeRties:::.ls_sanitize_job_id("../../other/jobs/x"), "Invalid job id")
  expect_error(LibeRties:::.ls_sanitize_job_id("/etc/passwd"), "Invalid job id")
  expect_equal(LibeRties:::.ls_sanitize_job_id("job-2026.01_abc"), "job-2026.01_abc")
})

test_that("submitted model/data allowlist rejects bad objects", {
  expect_error(LibeRties:::.ls_validate_submitted_model(list()), "nm_model")
  expect_error(LibeRties:::.ls_validate_submitted_data(list(x = 1)), "data.frame")
  m <- structure(list(), class = "nm_model")
  expect_silent(LibeRties:::.ls_validate_submitted_model(m))
  expect_silent(LibeRties:::.ls_validate_submitted_data(data.frame(ID = 1L)))
})

test_that("encryption-enabled submit refuses without cached user key", {
  skip_if_not_installed("sodium")
  root <- tempfile("ls_enc_refuse_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root, encrypt_at_rest = TRUE)
  LibeRties:::.ls_init_storage()
  ls_user_create("alice")
  sim <- if (requireNamespace("LibeRation", quietly = TRUE)) {
    LibeRation::nm_synthetic_theo(n_sub = 1L, seed = 1L)
  } else {
    skip("LibeRation not available")
  }
  raw_m <- serialize(sim$model, NULL)
  raw_d <- serialize(sim$data, NULL)
  payload <- list(
    job_type = "est",
    method = "FO",
    model_b64 = jsonlite::base64_enc(raw_m),
    data_b64 = jsonlite::base64_enc(raw_d),
    est_args = list(control = list(maxit = 2L))
  )
  expect_error(
    LibeRties:::.ls_job_submit("alice", list(max_concurrent_jobs = 1L), payload),
    "encryption key"
  )
})

test_that("est_args exceeding server maxima are rejected", {
  root <- tempfile("ls_estlim_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(
      sandbox_root = old_cfg$sandbox_root,
      max_est_maxit = old_cfg$max_est_maxit %||% 5000L
    )
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root, max_est_maxit = 10L)
  args <- list(control = list(maxit = 999L))
  expect_error(LibeRties:::.ls_enforce_est_limits(args), "maxit exceeds")
})

test_that("stale job results are not trusted after relaunch", {
  root <- tempfile("ls_recon_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root)
  LibeRties:::.ls_init_storage()
  jp <- LibeRties:::.ls_job_path("alice", "job1")
  dir.create(jp, recursive = TRUE)
  writeLines("old error", file.path(jp, "error.txt"))
  saveRDS(list(objective = 1), file.path(jp, "result.rds"))
  meta <- list(status = "running", started = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  expect_false(LibeRties:::.ls_job_result_trustworthy(meta, jp))
})

test_that("token rotation re-wraps encrypted job keys", {
  skip_if_not_installed("sodium")
  root <- tempfile("ls_rotate_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root, encrypt_at_rest = TRUE)
  LibeRties:::.ls_init_storage()
  u <- ls_user_create("alice"); tok <- u$token

  salt <- LibeRties:::.ls_user_enc_salt("alice")
  uk_old <- LibeRties:::.ls_derive_uk(tok, salt)
  jp <- LibeRties:::.ls_job_path("alice", "job_x"); dir.create(jp, recursive = TRUE)
  dek <- LibeRties:::.ls_dek_generate()
  LibeRties:::.ls_save_rds_safe(LibeRties:::.ls_dek_wrap(dek, uk_old),
                                file.path(jp, "key.enc"))
  secret <- list(objective = 7.0)
  LibeRties:::.ls_encrypt_to_file(secret, dek, file.path(jp, "result.enc"))

  # Refuse to orphan encrypted data.
  expect_error(ls_user_issue_token("alice"), "encrypted jobs")

  new_tok <- ls_user_issue_token("alice", current_token = tok)
  expect_false(identical(new_tok, tok))

  LibeRties:::.ls_uk_remember("alice", new_tok)
  expect_identical(LibeRties:::.ls_read_result(jp, "alice"), secret)

  # Old key no longer works.
  env <- LibeRties:::.ls_read_rds_safe(file.path(jp, "key.enc"))
  expect_error(LibeRties:::.ls_dek_unwrap(env, uk_old))
})

test_that("concurrency is per-user (serial native off by default)", {
  root <- tempfile("ls_conc_")
  dir.create(root)
  old_cfg <- ls_config()
  withr::defer({
    ls_config_set(sandbox_root = old_cfg$sandbox_root)
    unlink(root, recursive = TRUE)
  })
  ls_config_set(sandbox_root = root, worker_serial_native = FALSE)
  lim <- list(max_concurrent_jobs = 4L)
  expect_equal(LibeRties:::.ls_effective_max_concurrent(lim), 4L)
  ls_config_set(worker_serial_native = TRUE)
  expect_equal(LibeRties:::.ls_effective_max_concurrent(lim), 1L)
})
