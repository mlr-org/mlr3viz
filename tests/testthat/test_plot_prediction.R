context("plot_prediction")

test_that("plot_prediction.LearnerClassif", {
  task = tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))

  # predict_type = "prob"
  learner = lrn("classif.rpart", predict_type = "prob")$train(task)
  p = plot_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))

  # predict_type = "response"
  learner = lrn("classif.rpart", predict_type = "response")$train(task)
  p = plot_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
})

test_that("plot_prediction.LearnerRegr 2d", {
  task = tsk("boston_housing")$select(c("age", "dis"))
  learner = lrn("regr.rpart")$train(task)
  p = plot_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
})

test_that("plot_prediction.LearnerRegr 1d", {
  task = tsk("boston_housing")$select(c("age"))
  learner = lrn("regr.rpart")$train(task)
  p = plot_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
})
