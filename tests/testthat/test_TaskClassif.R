test_that("autoplot.TaskClassif", {
  task = mlr_tasks$get("iris")

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")
})
