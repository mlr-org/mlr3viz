context("fortify.Task")

test_that("fortify.Task", {
  expect_data_table(fortify(mlr_tasks$get("iris")), nrow = 150, ncol = 5)
  expect_data_table(fortify(mlr_tasks$get("boston_housing")))
})
