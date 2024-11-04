skip_if_not_installed("mlr3")

test_that("plot_learner_prediction.LearnerClassif", {
  task = mlr3::tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))

  # predict_type = "response"
  learner = mlr3::lrn("classif.rpart", predict_type = "response")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_response", p)

  # predict_type = "prob"
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_prob", p)

  # two-class, predict_type = "response"
  task = mlr3::tsk("sonar")$select(c("V1", "V2"))
  learner = lrn("classif.rpart", predict_type = "response")$train(task)
  p = plot_learner_prediction(learner, task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_binary_response", p)

  # two-class, predict_type = "prob"
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")$train(task)
  p = plot_learner_prediction(learner, task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_binary_prob", p)

  # categorical columns
  task = mlr3::tsk("german_credit")$select(c("housing", "employment_duration"))
  p = plot_learner_prediction(learner, task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_categorical", p)
})

test_that("plot_learner_prediction.LearnerRegr 2d", {
  task = mlr3::tsk("mtcars")$select(c("am", "carb"))
  learner = mlr3::lrn("regr.rpart")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_2d", p)
})

test_that("plot_learner_prediction.LearnerRegr 1d", {
  task = mlr3::tsk("mtcars")$select("am")

  # predict_type = "response"
  learner = mlr3::lrn("regr.rpart")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_1d_response", p)

  # predict_type = "se"
  learner = mlr3::lrn("regr.featureless", predict_type = "se")$train(task)
  p = plot_learner_prediction(learner, task, expand_range = 0.1)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_prediction_1d_se", p)
})
