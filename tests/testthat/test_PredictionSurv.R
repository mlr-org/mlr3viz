test_that("autoplot.PredictionSurv", {
  skip_if_not_installed("mlr3proba")
  require_namespaces("mlr3proba")

  task = mlr3::tsk("rats")$filter(1:100)
  learner = mlr3::lrn("surv.kaplan")$train(task)
  prediction = learner$predict(task)

  p = autoplot(prediction, type = "calib", task = task)
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "dcalib")
  expect_true(is.ggplot(p))
})
