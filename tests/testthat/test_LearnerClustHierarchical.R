skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3cluster")
skip_if_not_installed("factoextra")
set.seed(42)

test_that("autoplot.LearnerClustHierarchical", {
  require_namespaces("mlr3cluster")

  learner = mlr3::lrn("clust.agnes")$train(mlr3::tsk("usarrests"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_clust.agnes", p)

  learner = mlr3::lrn("clust.hclust")$train(mlr3::tsk("usarrests"))
  p = autoplot(learner, type = "scree")
  expect_true(is.ggplot(p))
  expect_doppelganger("learner_clust.hclust", p)
})
