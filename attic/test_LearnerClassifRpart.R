context("LearnerClassifRpart")

skip_if_not_installed("partykit")
skip_if_not_installed("ggparty")

test_that("autoplot.LearnerClassifRpart", {
  task = tsk("iris")
  learner = lrn("classif.rpart")$train(task)
  p = autoplot(learner)
  expect_true(is.ggplot(p))
})
