context("LearnerRegrRpart")

skip_if_not_installed("partykit")
skip_if_not_installed("ggparty")

test_that("autoplot.LearnerRegrRpart", {
  learner = lrn("regr.rpart")$train(tsk("mtcars"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
})
