test_that("autoplot.TaskClust", {
  library(mlr3cluster)
  task = mlr_tasks$get("usarrests")

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")
})
