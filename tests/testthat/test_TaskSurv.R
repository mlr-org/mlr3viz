test_that("autoplot.TaskSurv", {
  skip_if_not_installed("mlr3proba")

  require_namespaces("mlr3proba")
  task = mlr3::tsk("rats")

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")
})
