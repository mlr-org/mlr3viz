test_that("autoplot.PredictionClassif decision boundary probability", {
  set.seed(42)
  task = mlr3::tsk("pima")$select(c("age", "pedigree"))
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")$train(task)

  p = autoplot(learner, type = "prediction", task = task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_classif_prob", p)
})

test_that("autoplot.PredictionClassif decision boundary response", {
  set.seed(42)
  task = mlr3::tsk("pima")$select(c("age", "pedigree"))
  learner = mlr3::lrn("classif.rpart")$train(task)

  p = autoplot(learner, type = "prediction", task = task)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_classif_response", p)
})
