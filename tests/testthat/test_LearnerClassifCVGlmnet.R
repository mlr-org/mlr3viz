context("LearnerClassifCVGlmnet")

skip_if_not_installed("mlr3learners")
skip_if_not_installed("glmnet")
skip_if_not_installed("ggfortify")

test_that("autoplot.LearnerClassifCVGlmnet", {
  requireNamespace("mlr3learners")
  learner = mlr3::lrn("classif.cv_glmnet")$train(mlr3::tsk("wine"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
})
