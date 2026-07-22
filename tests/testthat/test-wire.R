test_that("JSON wire jobs rebuild models rather than trusting expression IR", {
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT"), ADVAN = 1,
    PRED = "CL=THETA(1)\nV=THETA(2)\nS1=V", ERROR = "Y=F",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20))
  )
  data <- data.frame(
    ID = c("one", "one"), TIME = c(0, 1), EVID = c(1L, 0L),
    AMT = c(100, 0), DV = c(NA_real_, 4.5), stringsAsFactors = FALSE
  )
  job <- ls_job("simulate", model, data, arguments = list(theta = c(2, 20)))
  encoded <- ls_job_encode(job)
  expect_false(grepl("pred_ir", encoded, fixed = TRUE))
  rebuilt <- ls_job_decode(encoded)
  expect_identical(ls_job_to_wire(job)$version, 2L)
  expect_s3_class(rebuilt$model, "nm_model")
  expect_s3_class(rebuilt$model$pred_ir, "libertad_ir")
  expect_identical(rebuilt$data$ID, data$ID)
  expect_equal(rebuilt$data$DV, data$DV)
  expect_equal(
    LibeRation::nm_simulate(rebuilt$model, rebuilt$data)$IPRED,
    LibeRation::nm_simulate(model, data)$IPRED
  )
})

test_that("wire v2 retains first-class advanced model semantics", {
  skip_if_not_installed("LibeRation", minimum_version = "0.8.0")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "DV", "MDV", "DVID"), ADVAN = 1,
    PRED = "CL=1;V=1;S1=V;F=0",
    ERROR = paste(
      "I1=0.6", "I2=0.4", "T11=0.8", "T12=0.2", "T21=0.3", "T22=0.7",
      "E1=ifelse(DV==0,0.9,0.1)", "E2=ifelse(DV==0,0.2,0.8)", sep = "\n"
    ),
    THETAS = data.frame(THETA = 1, Value = 1, FIX = TRUE),
    HMM_CONFIG = LibeRation::nm_hmm_config(
      states = c("low", "high"), initial = c("I1", "I2"),
      transition = matrix(c("T11", "T12", "T21", "T22"), 2, byrow = TRUE),
      emission = c("E1", "E2"), by_dvid = FALSE
    )
  )
  data <- data.frame(ID = 1, TIME = 0:1, DV = c(0, 1), MDV = 0L, DVID = 1L)
  rebuilt <- ls_job_decode(ls_job_encode(ls_job("estimate", model, data)))
  expect_s3_class(rebuilt$model$HMM_CONFIG, "nm_hmm_config")
  expect_equal(rebuilt$model$HMM_CONFIG$transition, model$HMM_CONFIG$transition)
  expect_identical(rebuilt$model$HMM_CONFIG$states, c("low", "high"))
})

test_that("wire transport preserves sequential estimation stages and outputs", {
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME"), OUTPUT = "CL", ADVAN = 1,
    PRED = "CL=THETA(1); V=THETA(2); S1=V", ERROR = "Y=F",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20))
  )
  stages <- list(
    LibeRation::nm_est_stage("FO", maxit = 1),
    LibeRation::nm_est_stage("FOCE", maxit = 1)
  )
  rebuilt <- ls_job_decode(ls_job_encode(ls_job(
    "estimate_sequence", model, data.frame(ID = 1, TIME = 0),
    arguments = list(stages = stages)
  )))
  expect_equal(rebuilt$type, "estimate_sequence")
  expect_equal(rebuilt$model$OUTPUT, "CL")
  expect_equal(
    vapply(rebuilt$arguments$stages, `[[`, character(1), "method"),
    c("FO", "FOCE")
  )
})

test_that("JSON wire preserves likelihood and mixture semantics", {
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME"), ADVAN = 1,
    PRED = "CL=ifelse(MIXNUM==1,THETA(1),THETA(2));V=THETA(3)",
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:3, Value = c(1, 2, 10)),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.1),
    LIK_CONFIG = LibeRation::nm_lik_config(
      error = "additive", mixtures = LibeRation::nm_mixture(c(0.7, 0.3), c("slow", "fast"))
    )
  )
  job <- ls_job("simulate", model, data.frame(ID = 1, TIME = 0))
  rebuilt <- ls_job_decode(ls_job_encode(job))
  expect_s3_class(rebuilt$model$LIK_CONFIG, "nm_lik_config")
  expect_s3_class(rebuilt$model$LIK_CONFIG$mixtures, "nm_mixture")
  expect_equal(rebuilt$model$LIK_CONFIG$mixtures$probability, c(0.7, 0.3))
  expect_equal(rebuilt$model$LIK_CONFIG$mixtures$label, c("slow", "fast"))
})

test_that("JSON wire reconstructs arbitrary matrix graph semantics", {
  skip_if_not_installed("LibeRation")
  graph <- LibeRation::nm_matrix_model(
    data.frame(id = 1:2, name = c("CENTRAL", "PERIPHERAL"),
               volume_parameter = c("VC", "VP")),
    data.frame(from = c(1, 1, 2), to = c(0, 2, 1),
               type = "clearance", parameter = c("CL", "Q", "Q"))
  )
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT"), ADVAN = 3, SOLVER = "matrix",
    GRAPH = graph, PRED = "CL=THETA(1);VC=THETA(2);Q=THETA(3);VP=THETA(4)",
    ERROR = "Y=F", THETAS = data.frame(THETA = 1:4, Value = c(2, 20, 1, 10))
  )
  data <- data.frame(ID = 1, TIME = c(0, 2), EVID = c(1, 0), AMT = c(100, 0))
  rebuilt <- ls_job_decode(ls_job_encode(ls_job("simulate", model, data)))
  expect_s3_class(rebuilt$model$GRAPH, "nm_matrix_model")
  expect_equal(LibeRation::nm_simulate(rebuilt$model, data)$IPRED,
               LibeRation::nm_simulate(model, data)$IPRED, tolerance = 1e-12)
})

test_that("wire transport rejects executable objects and invalid semantic fields", {
  expect_error(LibeRties:::.ls_wire_pack(list(run = identity)), "executable")
  payload <- list(
    schema = "liber.job.wire", version = 1L, type = "simulate", label = "",
    created = "now", model = LibeRties:::.ls_wire_pack(list(evil = 1)),
    data = LibeRties:::.ls_wire_pack(data.frame(ID = 1, TIME = 0)),
    arguments = LibeRties:::.ls_wire_pack(list())
  )
  expect_error(ls_job_from_wire(payload), "invalid semantic fields")
})

test_that("result wire preserves matrices, names, and missing values", {
  result <- list(
    objective = 12.5, eta = matrix(c(1, NA, 3, 4), 2, 2,
                                  dimnames = list(c("a", "b"), c("e1", "e2"))),
    state = c(status = "completed")
  )
  decoded <- ls_result_decode(ls_result_encode(result))
  expect_equal(decoded, result)
})

test_that("result wire retains supported result metadata", {
  result <- data.frame(IPRED = c(1, 2))
  attr(result, "solver") <- "advan"
  attr(result, "state_names") <- "CENTRAL"
  decoded <- ls_result_decode(ls_result_encode(result))
  expect_s3_class(decoded, "data.frame")
  expect_equal(attr(decoded, "solver"), "advan")
  expect_equal(attr(decoded, "state_names"), "CENTRAL")
})

test_that("LibeRality results round-trip through the result contract", {
  skip_if_not_installed("LibeRality", minimum_version = "0.2.0")
  evaluated <- LibeRality::lity_evaluate(
    LibeRality::lity_example()$design, LibeRality::lity_criterion_D()
  )
  decoded <- ls_result_decode(ls_result_encode(evaluated))
  expect_s3_class(decoded, "lity_evaluation")
  expect_s3_class(decoded$design, "lity_design")
  expect_true(all(vapply(decoded$design$arms, inherits, logical(1), "lity_arm")))
})

test_that("remote fitted results rebuild nested model and dataset classes", {
  skip_if_not_installed("LibeRation")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "DV", "MDV"), ADVAN = 1,
    PRED = "CL=THETA(1)*exp(ETA(1));V=THETA(2);S1=V",
    ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20), FIX = TRUE),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.1, FIX = TRUE),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.2, FIX = TRUE)
  )
  data <- data.frame(
    ID = rep(1:2, each = 3), TIME = rep(c(0, 1, 4), 2),
    EVID = rep(c(1, 0, 0), 2), AMT = rep(c(100, 0, 0), 2),
    DV = c(NA, 4.5, 3.2, NA, 4.3, 3.0), MDV = rep(c(1, 0, 0), 2)
  )
  fit <- LibeRation::nm_est(model, data, method = "FOCEI", eta_maxit = 60)
  decoded <- ls_result_decode(ls_result_encode(fit))
  expect_s3_class(decoded, "nm_fit")
  expect_s3_class(decoded$model, "nm_model")
  expect_s3_class(decoded$data, "nm_dataset")
  expect_equal(predict(decoded)$IPRED, predict(fit)$IPRED, tolerance = 1e-10)
})

test_that("individualisation jobs retain the typed LibeRator contract", {
  skip_if_not_installed("LibeRation", minimum_version = "0.6.1")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "DV", "MDV"), ADVAN = 1,
    PRED = "CL=THETA(1)*exp(ETA(1));V=THETA(2);S1=V", ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20)),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.1),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.2)
  )
  data <- data.frame(ID = 1, TIME = c(0, 1), EVID = c(1, 0), AMT = c(100, 0),
                     DV = c(NA, 4.5), MDV = c(1, 0))
  job <- ls_job("individualise", model, data)
  decoded <- ls_job_decode(ls_job_encode(job))
  expect_identical(decoded$type, "individualise")
  expect_s3_class(decoded$model, "nm_model")
  expect_true("LibeRator" %in% names(ls_job_manifest(job)$requirements))
  expect_true("individualise" %in% ls_queue_capabilities()$job_types)
})

test_that("remote individual-fit results rebuild model and dataset semantics", {
  skip_if_not_installed("LibeRation", minimum_version = "0.6.1")
  model <- LibeRation::nm_model(
    INPUT = c("ID", "TIME", "EVID", "AMT", "DV", "MDV"), ADVAN = 1,
    PRED = "CL=THETA(1)*exp(ETA(1));V=THETA(2);S1=V", ERROR = "Y=F+ERR(1)",
    THETAS = data.frame(THETA = 1:2, Value = c(2, 20)),
    OMEGAS = data.frame(OMEGA = 1, Value = 0.1),
    SIGMAS = data.frame(SIGMA = 1, Value = 0.2)
  )
  data <- data.frame(ID = 1, TIME = c(0, 1, 4), EVID = c(1, 0, 0),
                     AMT = c(100, 0, 0), DV = c(NA, 4.5, 3.2), MDV = c(1, 0, 0))
  fit <- LibeRation::nm_individual_fit(model, data)
  decoded <- ls_result_decode(ls_result_encode(fit))
  expect_s3_class(decoded, "nm_individual_fit")
  expect_s3_class(decoded$model, "nm_model")
  expect_s3_class(decoded$data, "nm_dataset")
  expect_equal(decoded$eta, unname(fit$eta))
  expect_equal(decoded$eta_covariance, fit$eta_covariance)
})
