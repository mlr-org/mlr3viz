set.seed(42)
task = mlr3::tsk("sonar")
learner = mlr3::lrn("classif.rpart", predict_type = "prob")$train(task)
prediction = learner$predict(task)

test_that("autoplot.PredictionClassif", {
  p = autoplot(prediction, type = "stacked")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("predictionclassif_stacked", p)

  p = autoplot(prediction, type = "roc")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("predictionclassif_roc", p)

  p = autoplot(prediction, type = "prc")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("predictionclassif_prc", p)

  p = autoplot(prediction, type = "threshold")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("predictionclassif_threshold", p)
})

test_that("roc is not inverted", {
  skip_if_not_installed("precrec")
  tab = as.data.table(precrec::auc(precrec::evalmod(as_precrec(prediction))))
  expect_numeric(tab[curvetypes == "ROC", aucs], len = 1L, lower = 0.5)
})
