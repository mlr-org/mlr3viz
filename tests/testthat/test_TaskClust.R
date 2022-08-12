skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3cluster")

test_that("autoplot.TaskClust", {
  requireNamespace("mlr3cluster")
  task = mlr3::tsk("usarrests")

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")
  expect_doppelganger("taskclust_pairs", p)
})
