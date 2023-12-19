test_that("autoplot.PredictionRegr decision boundary 1D", {
  set.seed(42)
  task = mlr3::tsk("mtcars")$select("am")
  learner = mlr3::lrn("regr.rpart")$train(task)

  p = autoplot(learner, type = "prediction", task = task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_regression_1D", p)
})

test_that("autoplot.PredictionClassif decision boundary 2D", {
  set.seed(42)
  task = mlr3::tsk("mtcars")$select(c("am", "carb"))
  learner = mlr3::lrn("regr.rpart")$train(task)

  p = autoplot(learner, type = "prediction", task = task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_regression_2D", p)
})

test_that("autoplot.PredictionClassif decision boundary 2D", {
  set.seed(42)
  task = mlr3::tsk("mtcars")$select("am")
  learner = mlr3::lrn("regr.featureless", predict_type = "se")$train(task)

  p = autoplot(learner, type = "prediction", task = task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_regression_2D_se", p)
})

