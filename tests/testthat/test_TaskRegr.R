context("TaskRegr")


test_that("autoplot.TaskRegr", {
  task = mlr_tasks$get("mtcars")$select(c("carb", "cyl"))

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_is(p, "ggmatrix")
})
