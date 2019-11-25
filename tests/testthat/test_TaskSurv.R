context("TaskSurv")


test_that("autoplot.TaskSurv", {
  requireNamespace("mlr3proba")
  task = mlr_tasks$get("rats")
  p = autoplot(task)
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_is(p, "ggmatrix")
})
