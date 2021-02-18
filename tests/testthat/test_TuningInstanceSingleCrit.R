test_that("fortify.TuningInstanceSingleCrit", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3tuning")
  skip_if_not_installed("patchwork")
  library(mlr3tuning) #nolint

  learner = lrn("classif.rpart")
  learner$param_set$values$cp = to_tune(0.001, 0.1)
  learner$param_set$values$minsplit = to_tune(1, 10)
 
  instance = TuningInstanceSingleCrit$new(
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4))
 
  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  f = fortify(instance)
  expect_data_table(f, nrows = 4, ncols = 8)
  expect_names(names(f), permutation.of = c("cp", "minsplit", "classif.ce", "uhash", "timestamp", "batch_nr", 
    "x_domain_cp", "x_domain_minsplit"))
})

test_that("autoplot.TuningInstanceSingleCrit", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("mlr3tuning")
  skip_if_not_installed("ranger")
  skip_if_not_installed("xgboost")
  skip_if_not_installed("patchwork")
  library(mlr3learners) #nolint
  library(mlr3tuning) #nolint

  learner = lrn("classif.rpart")
  learner$param_set$values$cp = to_tune(0.001, 0.1)
  learner$param_set$values$minsplit = to_tune(1, 10)
 
  instance = TuningInstanceSingleCrit$new(
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4))
 
  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  p = autoplot(instance, type = "marginal")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "marginal", cols_x = "x_domain_cp")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "marginal", trafo = TRUE)
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "performance")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "parameter")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "parameter", cols_x = "x_domain_cp")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "parameter", trafo = TRUE)
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "surface")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "surface", grid_resolution = 50)
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "surface", learner = lrn("regr.lm"))
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "parallel")
  expect_true(is.ggplot(p))

  learner = lrn("classif.xgboost")
  learner$param_set$values$eta = to_tune(0.01, 0.1)
  learner$param_set$values$nrounds = to_tune(1, 2)
  learner$param_set$values$booster = to_tune()
  learner$param_set$values$maximize = to_tune()

  instance = TuningInstanceSingleCrit$new(
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4))
 
  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  p = autoplot(instance, type = "parallel")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "parallel", cols_x = c("maximize", "booster"))
  expect_true(is.ggplot(p))

  expect_error(autoplot(instance, type = "surface"), 
    regexp = "Surface plots can only be drawn with 2 parameters.",
    fixed = TRUE)

  expect_error(autoplot(instance, type = "surface", cols_x = "nrounds"), 
    regexp = "Surface plots can only be drawn with 2 parameters.",
    fixed = TRUE)
})
