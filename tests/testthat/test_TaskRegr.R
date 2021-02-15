test_that("autoplot.TaskRegr", {
  task = mlr3::tsk("mtcars")$select(c("carb", "cyl"))

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")
})
