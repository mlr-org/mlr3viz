skip_on_os("solaris")
skip_if_not_installed("mlr3learners")
skip_if_not_installed("glmnet")
skip_if_not_installed("ggfortify")
set.seed(42)

test_that("autoplot.LearnerClassifGlmnet", {
  requireNamespace("mlr3learners")
  learner = mlr3::lrn("classif.glmnet")$train(mlr3::tsk("sonar"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("learner_classif.glmnet", p)
})
