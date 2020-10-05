context("TaskClust")

test_that("autoplot.TaskClust", {
  task = mlr_tasks$get("usarrests") 
  
  p = autoplot(task, type = "pairs")
  expect_is(p, "ggmatrix")
})
