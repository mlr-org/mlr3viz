skip_on_os("solaris")
skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3learners")
skip_if_not_installed("glmnet")
skip_if_not_installed("ggfortify")

test_that("autoplot.LearnerRegrGlmnet", {
  requireNamespace("mlr3learners")
  learner = mlr3::lrn("regr.glmnet")$train(mlr3::tsk("mtcars"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_regr.glmnet", p)
})
