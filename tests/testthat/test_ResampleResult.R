context("ResampleResult")

library(mlr3)
task = tsk("sonar")
learner = lrn("classif.rpart", predict_type = "prob")
resampling = rsmp("cv")
rr = resample(task, learner, resampling)

test_that("fortify ResampleResult", {
  f = fortify(rr, measure = msr("classif.ce"))
  expect_data_table(f, nrows = 10, ncols = 3)
  expect_names(names(f), identical.to = c("iteration", "measure_id", "performance"))
})

test_that("autoplot ResampleResult", {
  p = autoplot(rr, measure = msr("classif.ce"), type = "boxplot")
  expect_true(is.ggplot(p))

  p = autoplot(rr, measure = msr("classif.ce"), type = "histogram")
  expect_true(is.ggplot(p))

  p = autoplot(rr, type = "roc")
  expect_true(is.ggplot(p))

  p = autoplot(rr, type = "prc")
  expect_true(is.ggplot(p))
})

test_that("autoplot ResampleResult type=prediction", {
  # classif prob
  task2 = tsk("sonar")$select(c("V1", "V2"))
  rr2 = resample(task2, learner, resampling, store_models = TRUE)
  autoplot(rr2, type = "prediction")

  # regr 1d se
  task3 = tsk("boston_housing")$select(c("age"))
  rr3 = resample(task3, lrn("regr.rpart"), resampling, store_models = TRUE)
  autoplot(rr3, type = "prediction")
})
