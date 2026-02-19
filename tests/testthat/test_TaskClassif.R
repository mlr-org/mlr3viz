skip_if_not_installed("mlr3")

test_that("autoplot.TaskClassif", {
  task = mlr3::tsk("iris")

  p = autoplot(task, type = "target")
  expect_true(is_ggplot(p))
  expect_doppelganger("taskclassif_target", p)

  skip_if_not_installed("GGally")
  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")
  expect_doppelganger("taskclassif_pairs", p)

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")
  expect_doppelganger("taskclassif_duo", p)
})
