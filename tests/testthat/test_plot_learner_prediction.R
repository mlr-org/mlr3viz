context("plot_learner_prediction")

test_that("plot_learner_prediction.LearnerClassif", {
  task = tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))

  # predict_type = "prob"
  learner = lrn("classif.rpart", predict_type = "prob")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))

  # predict_type = "response"
  learner = lrn("classif.rpart", predict_type = "response")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))

  # two-class, predict_type = "response"
  task = tsk("sonar")$select(c("V1", "V2"))
  learner = lrn("classif.rpart", predict_type = "response")$train(task)
  p = plot_learner_prediction(learner, task)
  expect_true(is.ggplot(p))

  # two-class, predict_type = "prob"
  learner = lrn("classif.rpart", predict_type = "prob")$train(task)
  p = plot_learner_prediction(learner, task)
  expect_true(is.ggplot(p))

  # non numerical columns
  task = tsk("german_credit")$select(c("housing", "employment_duration"))
  p = plot_learner_prediction(learner, task)
  expect_true(is.ggplot(p))
})

test_that("plot_learner_prediction.LearnerRegr 2d", {
  task = tsk("boston_housing")$select(c("age", "dis"))
  learner = lrn("regr.rpart")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
})

test_that("plot_learner_prediction.LearnerRegr 1d", {
  task = tsk("boston_housing")$select(c("age"))

  # predict_type = "response"
  learner = lrn("regr.rpart")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))

  # predict_type = "se"
  learner = lrn("regr.featureless", predict_type = "se")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
})
