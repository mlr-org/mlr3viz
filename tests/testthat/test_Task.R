context("fortify.Task")

test_that("fortify.Task", {
  expect_data_table(fortify(mlr_tasks$get("iris")), nrows = 150, ncols = 5)
  expect_data_table(fortify(mlr_tasks$get("boston_housing")))
})
