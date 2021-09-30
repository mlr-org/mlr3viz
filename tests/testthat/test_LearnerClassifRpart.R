skip_if_not_installed("rpart")
skip_if_not_installed("partykit")
skip_if_not_installed("ggparty")

test_that("autoplot.LearnerClassifRpart", {
  learner = mlr3::lrn("classif.rpart", keep_model = TRUE)$train(mlr3::tsk("iris"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("learner_classif.rpart", p)
})
