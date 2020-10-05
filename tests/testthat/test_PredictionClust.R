context("PredictionClust")

test_that("autoplot.PredictionClust", {
  skip_if_not_installed("mlr3cluster")

  require_namespaces("mlr3cluster")
  task = mlr_tasks$get("usarrests")
  learner = lrn("clust.kmeans", centers = 3)
  prediction = learner$train(task)$predict(task)

  p = autoplot(prediction, task, type = "scatter")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, task, type = "sil")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, task, type = "pca")
  expect_true(is.ggplot(p))
})
