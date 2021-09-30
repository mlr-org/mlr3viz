test_that("autoplot.TaskSurv", {
  skip_if_not_installed("mlr3proba")

  require_namespaces("mlr3proba")
  task = mlr3::tsk("rats")

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))
  # vdiffr::expect_doppelganger("tasksurv_target", p)

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")
  # vdiffr::expect_doppelganger("tasksurv_ggmatrix", p)

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")
  # vdiffr::expect_doppelganger("tasksurv_duo", p)
})
