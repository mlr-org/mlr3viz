context("PredictionRegr")

test_that("autoplot.PredictionRegr", {
  task = tsk("mtcars")$select(c("carb", "cyl"))
  learner = lrn("regr.rpart")$train(task)
  prediction = learner$predict(task)

  p = autoplot(prediction, type = "xy")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "histogram")
  expect_true(is.ggplot(p))
})
