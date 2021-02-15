test_that("autoplot.TaskClassif", {
  task = mlr3::tsk("iris")

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")
})
