context("PredictionRegr")

test_that("autoplot.PredictionRegr", {
  task = tsk("mtcars")$select(c("carb", "cyl"))
  learner = lrn("regr.rpart")$train(task)
  pred = learner$predict(task)

  p = autoplot(pred, type = "xy")
  expect_true(is.ggplot(p))

  p = autoplot(pred, type = "histogram")
  expect_true(is.ggplot(p))
})
