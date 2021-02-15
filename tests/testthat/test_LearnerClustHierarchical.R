skip_if_not_installed("mlr3cluster")
skip_if_not_installed("factoextra")

test_that("autoplot.LearnerClutsHierarchical", {
  require_namespaces("mlr3cluster")

  learner = mlr3::lrn("clust.agnes")$train(mlr3::tsk("usarrests"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
})
