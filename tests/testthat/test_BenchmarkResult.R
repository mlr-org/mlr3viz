context("BenchmarkResult")

tasks = mlr_tasks$mget(c("iris", "pima", "sonar"))
learner = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
resampling = mlr_resamplings$mget("cv")
bmr = logger::with_log_threshold(
  benchmark(expand_grid(tasks, learner, resampling)),
  logger::WARN, namespace = "mlr3")

test_that("fortify BenchmarkResult", {
  f = fortify(bmr)
  expect_data_table(f, nrow = 60, ncol = 5)
  expect_names(names(f), permutation.of = c("hash", "task_id", "learner_id", "resampling_id", "classif.mmce"))
})

test_that("autoplot BenchmarkResult", {
  p = autoplot(bmr)
  expect_true(is.ggplot(p))
})
