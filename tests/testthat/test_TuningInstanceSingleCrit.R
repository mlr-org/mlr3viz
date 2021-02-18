test_that("autoplot.TuningInstanceSingleCrit", {
  library(mlr3learners)

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

  learner$param_set$values$keep_model = to_tune()
 
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

  expect_error(autoplot(instance, type = "surface"), 
    regexp = "Surface plots can only be drawn with 2 parameters.",
    fixed = TRUE)

  expect_error(autoplot(instance, type = "surface", cols_x = "cp"), 
    regexp = "Surface plots can only be drawn with 2 parameters.",
    fixed = TRUE)
})