skip_if_not_installed("mlr3")
skip_if_not_installed("rpart")
skip_if_not_installed("partykit")
skip_if_not_installed("ggparty")

test_that("autoplot.LearnerClassifRpart", {
  learner = mlr3::lrn("classif.rpart", keep_model = TRUE)$train(mlr3::tsk("iris"))
  p = autoplot(learner, type = "ggparty")
  expect_true(is_ggplot(p))
  expect_doppelganger("learner_classif.rpart", p)
})
