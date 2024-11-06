skip_if_not_installed("mlr3")

test_that("fortify.Task", {
  expect_data_table(fortify(mlr3::tsk("iris")), nrows = 150, ncols = 5)
  expect_data_table(fortify(mlr3::tsk("mtcars")))
})
