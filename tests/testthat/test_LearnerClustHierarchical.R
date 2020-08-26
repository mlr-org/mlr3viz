context("LearnerHierarchical")

skip_if_not_installed("mlr3cluster")
skip_if_not_installed("factoextra")

test_that("autoplot.LearnerClutsHierarchical", {
  require_namespaces("mlr3cluster")

  learner = lrn("clust.agnes")$train(tsk("usarrests"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
})
