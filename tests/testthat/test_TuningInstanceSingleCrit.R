skip_if_not_installed("mlr3")
skip_if_not_installed("paradox")
skip_if_not_installed("mlr3tuning")
skip_if_not_installed("patchwork")
skip_if_not_installed("mlr3tuning")
skip_if_not_installed("mlr3learners")
library(mlr3tuning)
requireNamespace("mlr3learners")
set.seed(42)

learner = mlr3::lrn("classif.rpart")
learner$param_set$values$cp = paradox::to_tune(0.001, 0.1)
learner$param_set$values$minsplit = paradox::to_tune(1, 10)

instance = TuningInstanceSingleCrit$new(
  task = mlr3::tsk("iris"),
  learner = learner,
  resampling = mlr3::rsmp("holdout"),
  measure = mlr3::msr("classif.ce"),
  terminator = trm("evals", n_evals = 4))

tuner = tnr("random_search", batch_size = 2)
invoke(tuner$optimize, instance, .seed = 123)


test_that("fortify.TuningInstanceSingleCrit", {
  f = fortify(instance)
  expect_data_table(f, nrows = 4)
})

test_that("autoplot.TuningInstanceSingleCrit", {
  expect_single = function(id, plot) {
    expect_true(is.ggplot(plot))
    vdiffr::expect_doppelganger(sprintf("tisc_%s", id), plot)
  }

  expect_multiple = function(id, plots) {
    assert_string(id)
    expect_class(plots, "DelayedPatchworkPlot")
    for (i in seq_along(plots)) {
      cur = plots[[i]]
      expect_true(is.ggplot(cur))
      vdiffr::expect_doppelganger(sprintf("tisc_%s_%02i", id, i), cur)
    }
  }

  p = autoplot(instance, type = "marginal")
  expect_multiple("marginal", p)

  p = autoplot(instance, type = "marginal", cols_x = "x_domain_cp")
  expect_multiple("marginal_x_domain", p)

  p = autoplot(instance, type = "marginal", trafo = TRUE)
  expect_multiple("marginal_trafo", p)

  p = autoplot(instance, type = "performance")
  expect_single("performance", p)

  p = autoplot(instance, type = "parameter")
  expect_multiple("parameter", p)

  p = autoplot(instance, type = "parameter", cols_x = "x_domain_cp")
  expect_multiple("parameter_x_domain", p)

  p = autoplot(instance, type = "parameter", trafo = TRUE)
  expect_multiple("parameter_trafo", p)

  p = autoplot(instance, type = "parameter", return_list = TRUE)
  expect_multiple("parameter_return_list", p)

  p = autoplot(instance, type = "surface")
  expect_single("surface", p)

  p = autoplot(instance, type = "surface", grid_resolution = 50)
  expect_single("surface_grid_50", p)

  p = autoplot(instance, type = "surface", learner = lrn("regr.lm"))
  expect_single("surface_regr_lm", p)

  p = autoplot(instance, type = "points")
  expect_single("points", p)

  p = autoplot(instance, type = "parallel")
  expect_single("parallel", p)

  p = autoplot(instance, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  # with categoircal params

  learner = mlr3::lrn("classif.xgboost")
  learner$param_set$values$eta = paradox::to_tune(0.01, 0.1)
  learner$param_set$values$nrounds = paradox::to_tune(1, 2)
  learner$param_set$values$booster = paradox::to_tune()
  learner$param_set$values$maximize = paradox::to_tune()

  instance = TuningInstanceSingleCrit$new(
    task = mlr3::tsk("iris"),
    learner = learner,
    resampling = mlr3::rsmp("holdout"),
    measure = mlr3::msr("classif.ce"),
    terminator = trm("evals", n_evals = 4))

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  p = autoplot(instance, type = "parallel")
  expect_single("parallel_xgboost_1", p)

  p = autoplot(instance, type = "parallel", cols_x = c("maximize", "booster"))
  expect_true(is.ggplot(p))
  # expect_single("parallel_xgboost_1", p)

  expect_error(autoplot(instance, type = "surface"),
    regexp = "Surface plots can only be drawn with 2 parameters.",
    fixed = TRUE)

  expect_error(autoplot(instance, type = "surface", cols_x = "nrounds"),
    regexp = "Surface plots can only be drawn with 2 parameters.",
    fixed = TRUE)

  instance$archive$data[1, 1] = NA

  expect_error(autoplot(instance, type = "parallel"),
    regexp = "Parallel coordinate plots cannot be displayed with missing data.",
    fixed = TRUE)
})
