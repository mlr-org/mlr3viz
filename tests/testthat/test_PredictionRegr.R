test_that("autoplot.PredictionRegr", {
  task = mlr3::tsk("mtcars")$select(c("carb", "cyl"))
  learner = mlr3::lrn("regr.rpart")$train(task)
  prediction = learner$predict(task)

  p = autoplot(prediction, type = "xy")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "histogram")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "residual")
  expect_true(is.ggplot(p))
})
