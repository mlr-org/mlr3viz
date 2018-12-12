context("ResampleResult")

task = mlr_tasks$get("iris")
learner = mlr_learners$get("classif.rpart")
resampling = mlr_resamplings$get("cv")
rr = logger::with_log_threshold(
  resample(task, learner, resampling),
  logger::WARN, namespace = "mlr3")

test_that("fortify ResampleResult", {
  f = fortify(rr)
  expect_data_table(f, nrow = 10, ncol = 3)
  expect_names(names(f), identical.to = c("iteration", "measure_id", "performance"))
})

test_that("autoplot ResampleResult", {
  p = autoplot(rr)
  expect_true(is.ggplot(p))
})
