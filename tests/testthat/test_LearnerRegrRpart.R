skip_if_not_installed("rpart")
skip_if_not_installed("partykit")
skip_if_not_installed("ggparty")

test_that("autoplot.LearnerRegrRpart", {
  learner = mlr3::lrn("regr.rpart", keep_model = TRUE)$train(mlr3::tsk("mtcars"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
})
