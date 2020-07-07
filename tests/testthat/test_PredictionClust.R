context("PredictionClust")

test_that("autoplot.PredictionClust", {
  task = tsk("usarrests")
  learner = lrn("clust.kmeans", centers = 3)
  prediction = learner$train(task)$predict(task)

  p = autoplot(object, task, type = "scatter")
  expect_true(is.ggplot(p))
})
