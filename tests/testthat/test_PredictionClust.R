skip_if_not_installed("mlr3cluster")
skip_if_not_installed("clue")

test_that("autoplot.PredictionClust", {
  require_namespaces("mlr3cluster")
  task = mlr3::tsk("usarrests")
  learner = mlr3::lrn("clust.kmeans", centers = 3)
  prediction = learner$train(task)$predict(task)

  p = expect_warning(autoplot(prediction, task, type = "scatter"), "Factor variables are omitted")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, task, type = "sil")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, task, type = "pca")
  expect_true(is.ggplot(p))
})
