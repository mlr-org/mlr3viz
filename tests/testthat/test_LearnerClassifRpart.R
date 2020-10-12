context("LearnerClassifRpart")

skip_if_not_installed("rpart")
skip_if_not_installed("partykit")
skip_if_not_installed("ggparty")

test_that("autoplot.LearnerClassifRpart", {
  learner = lrn("classif.rpart", keep_model = TRUE)$train(tsk("iris"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
})
