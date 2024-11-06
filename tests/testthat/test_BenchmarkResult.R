skip_if_not_installed("mlr3")

set.seed(42)
tasks = mlr3::tsks(c("iris", "pima", "sonar"))
learner = mlr3::lrns(c("classif.featureless", "classif.rpart"), predict_type = "prob")
resampling = mlr3::rsmp("cv", folds = 3)
bmr = mlr3::benchmark(mlr3::benchmark_grid(tasks, learner, resampling))

test_that("fortify BenchmarkResult", {
  f = fortify(bmr, measure = msr("classif.ce"))
  expect_data_table(f, nrows = 18, ncols = 5)
  expect_names(names(f), permutation.of = c(
    "nr", "task_id", "learner_id",
    "resampling_id", "classif.ce"))
})

test_that("autoplot BenchmarkResult", {
  p = autoplot(bmr, measure = msr("classif.ce"), type = "boxplot")
  expect_true(is.ggplot(p))
  expect_doppelganger("bmr_boxplot", p)

  expect_error(autoplot(bmr, type = "roc"), "multiple")

  object = bmr$clone(deep = TRUE)$filter(task_ids = "sonar")
  p = autoplot(object, type = "roc")
  expect_true(is.ggplot(p))
  expect_doppelganger("bmr_roc", p)

  object = bmr$clone(deep = TRUE)$filter(task_ids = "pima")
  p = autoplot(object, type = "prc")
  expect_true(is.ggplot(p))
  expect_doppelganger("bmr_prc", p)
})

test_that("holdout roc plot (#54)", {
  tasks = tsks("german_credit")

  learners = c("classif.featureless", "classif.rpart")
  learners = lapply(learners, lrn,
    predict_type = "prob")

  resamplings = rsmp("holdout", ratio = .8) # holdout instead of cv

  design = benchmark_grid(tasks, learners, resamplings)
  bmr = benchmark(design)
  p = autoplot(bmr, type = "roc")
  expect_true(is.ggplot(p))

  expect_doppelganger("bmr_holdout_roc", p)
})

skip_if_not_installed("mlr3inferr")
skip_if_not_installed("rpart")

test_that("CI plot", {
  bmr = benchmark(benchmark_grid(tsks(c("mtcars", "mtcars")),
    lrns(c("regr.featureless", "regr.rpart")), rsmp("holdout")))

  p = autoplot(bmr, "ci", msr("ci", "regr.mse"))
  expect_true(is.ggplot(p))
  expect_doppelganger("bmr_holdout_ci", p)

  bmr = benchmark(benchmark_grid(tsk("iris"), lrn("classif.rpart"),
    rsmps(c("holdout", "cv"))))
  expect_error(autoplot(bmr, "ci", msr("ci", "classif.acc")), "one resampling method")
})
