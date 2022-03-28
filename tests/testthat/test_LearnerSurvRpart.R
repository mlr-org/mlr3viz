skip_if_not_installed("survival")
skip_if_not_installed("mlr3proba")
skip_if_not_installed("rpart")
skip_if_not_installed("partykit")
skip_if_not_installed("ggparty")

test_that("autoplot.LearnerSurvRpart", {
  learner = mlr3::lrn("surv.rpart")$train(mlr3::tsk("rats"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("learner_regr.rpart", p)
})
