skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3fselect")

test_that("autoplot ResampleResult", {
  require_namespaces(c("mlr3", "mlr3fselect"))

  result = data.table(
    resampling_iteration = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    learner_id = rep(c("classif.xgboost", "classif.rpart", "classif.ranger"), 3),
    n_features = c(2, 4, 4, 1, 5, 4, 1, 2, 4),
    features = list(
      c("V3", "V20"),
      c("V3", "V5", "V19", "V15"),
      c("V11", "V7", "V6", "V8"),
      c("V11"),
      c("V17", "V2", "V12", "V9", "V1"),
      c("V11", "V18", "V9", "V2"),
      c("V2"),
      c("V4", "V12"),
      c("V6", "V15", "V19", "V7")),
    classif.ce = c(0.13, 0.24, 0.16, 0.11, 0.25, 0.18, 0.15, 0.1, 0.16)
  )

  efsr = mlr3fselect::EnsembleFSResult$new(result = result, features = paste0("V", 1:20),
                                           measure = mlr3::msr("classif.ce"))

  # wrong type gives hint of types a user can input
  expect_error(autoplot(efsr, type = "XYZ"), regexp = "Must be element of set")

  # pareto (stepwise)
  p = autoplot(efsr)
  expect_true(is.ggplot(p))
  expect_doppelganger("pareto_stepwise", p)

  # pareto (estimated)
  p = autoplot(efsr, pareto_front = "estimated")
  expect_true(is.ggplot(p))
  expect_doppelganger("pareto_estimated", p)

  # Performance
  p = autoplot(efsr, type = "performance")
  expect_true(is.ggplot(p))
  expect_doppelganger("pareto_performance", p)

  # Number of features
  p = autoplot(efsr, type = "n_features")
  expect_true(is.ggplot(p))
  expect_doppelganger("pareto_n_features", p)

  # stability
  p = autoplot(efsr, type = "stability")
  expect_true(is.ggplot(p))
  expect_doppelganger("pareto_stability", p)
})
