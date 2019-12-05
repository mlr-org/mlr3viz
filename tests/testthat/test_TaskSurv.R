context("TaskSurv")


test_that("autoplot.TaskSurv", {
  skip_if_not_installed("mlr3proba")

  require_namespaces("mlr3proba")
  task = mlr_tasks$get("rats")

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_is(p, "ggmatrix")

  p = autoplot(task, type = "duo")
  expect_is(p, "ggmatrix")
})
