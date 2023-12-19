skip_on_os("solaris")
skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3learners")
skip_if_not_installed("glmnet")
skip_if_not_installed("ggfortify")
set.seed(42)

test_that("autoplot.LearnerClassifCVGlmnet", {
  requireNamespace("mlr3learners")
  learner = mlr3::lrn("classif.cv_glmnet")$train(mlr3::tsk("wine"))
  p = autoplot(learner, type = "ggfortify")
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_classif.cv_glmnet", p)
})
