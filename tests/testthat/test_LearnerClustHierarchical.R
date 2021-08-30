skip_if_not_installed("mlr3cluster")
skip_if_not_installed("factoextra")
set.seed(42)

test_that("autoplot.LearnerClustHierarchical", {
  require_namespaces("mlr3cluster")

  learner = mlr3::lrn("clust.agnes")$train(mlr3::tsk("usarrests"))
  p = autoplot(learner)
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("learner_clust.agnes", p)
})
