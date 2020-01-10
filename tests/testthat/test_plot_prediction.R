context("plot_prediction")

test_that("plot_prediction.LearnerClassif", {
  task = tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))
  learner = lrn("classif.rpart", predict_type = "prob")$train(task)
  p = plot_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
})
