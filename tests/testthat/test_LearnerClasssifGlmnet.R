skip_on_os("solaris")
skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3learners")
skip_if_not_installed("glmnet")
skip_if_not_installed("ggfortify")
set.seed(42)

test_that("autoplot.LearnerClassifGlmnet", {
  requireNamespace("mlr3learners")
  learner = mlr3::lrn("classif.glmnet")$train(mlr3::tsk("sonar"))
  p = autoplot(learner, type = "ggfortify")
  expect_true(is_ggplot(p))
  expect_doppelganger("learner_classif.glmnet", p)
})
