context("PredictionClassif")

test_that("autoplot.PredictionClassif", {
  task = tsk("sonar")
  learner = lrn("classif.rpart", predict_type = "prob")$train(task)
  prediction = learner$predict(task)

  p = autoplot(prediction, type = "stacked")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "roc")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "prc")
  expect_true(is.ggplot(p))
})
