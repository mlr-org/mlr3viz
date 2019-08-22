context("BenchmarkResult")

tasks = mlr_tasks$mget(c("iris", "pima", "sonar"))
learner = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resampling = mlr_resamplings$mget("cv")
bmr = benchmark(benchmark_grid(tasks, learner, resampling))

test_that("fortify BenchmarkResult", {
  f = fortify(bmr, measure = msr("classif.ce"))
  expect_data_table(f, nrows = 60, ncols = 5)
  expect_names(names(f), permutation.of = c("nr", "task_id", "learner_id", "resampling_id", "classif.ce"))
})

test_that("autoplot BenchmarkResult", {
  p = autoplot(bmr, measure = msr("classif.ce"))
  expect_true(is.ggplot(p))
})
