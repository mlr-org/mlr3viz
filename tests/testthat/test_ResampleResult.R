context("ResampleResult")

task = mlr_tasks$get("iris")
learner = mlr_learners$get("classif.rpart")
resampling = mlr_resamplings$get("cv")
rr = resample(task, learner, resampling)

test_that("fortify ResampleResult", {
  f = fortify(rr, measure = "classif.ce")
  expect_data_table(f, nrows = 10, ncols = 3)
  expect_names(names(f), identical.to = c("iteration", "measure_id", "performance"))
})

test_that("autoplot ResampleResult", {
  p = autoplot(rr, measure = "classif.ce")
  expect_true(is.ggplot(p))
})
