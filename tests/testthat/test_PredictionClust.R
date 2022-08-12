skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3cluster")
skip_if_not_installed("clue")

set.seed(42)

test_that("autoplot.PredictionClust", {
  require_namespaces("mlr3cluster")
  task = mlr3::tsk("usarrests")
  learner = mlr3::lrn("clust.kmeans", centers = 3)
  prediction = learner$train(task)$predict(task)

  expect_warning(p <- autoplot(prediction, task, type = "scatter"), "Factor variables are omitted")
  expect_true(is.ggplot(p))
  expect_doppelganger("predictionclust_scatter", p)

  p = autoplot(prediction, task, type = "sil")
  expect_true(is.ggplot(p))
  expect_doppelganger("predictionclust_sil", p)

  p = autoplot(prediction, task, type = "pca")
  expect_true(is.ggplot(p))
  expect_doppelganger("predictionclust_pca", p)
})
