context("BenchmarkResult")

library(mlr3)
tasks = tsks(c("iris", "pima", "sonar"))
learner = lrns(c("classif.featureless", "classif.rpart"), predict_type = "prob")
resampling = rsmp("cv", folds = 3)
bmr = benchmark(benchmark_grid(tasks, learner, resampling))

test_that("fortify BenchmarkResult", {
  f = fortify(bmr, measure = msr("classif.ce"))
  expect_data_table(f, nrows = 18, ncols = 5)
  expect_names(names(f), permutation.of = c(
    "nr", "task_id", "learner_id",
    "resampling_id", "classif.ce"))
})

test_that("autoplot BenchmarkResult", {
  p = autoplot(bmr, measure = msr("classif.ce"))
  expect_true(is.ggplot(p))

  expect_error(autoplot(bmr, type = "roc"), "multiple")

  object = bmr$clone()$filter(task_ids = "sonar")
  p = autoplot(object, type = "roc")
  expect_true(is.ggplot(p))

  object = bmr$clone()$filter(task_ids = "pima")
  p = autoplot(object, type = "prc")
  expect_true(is.ggplot(p))
})
