skip_if_not_installed("mlr3")

test_that("autoplot.Filter", {
  task = mlr3::tsk("iris")
  f = mlr3filters::mlr_filters$get("anova")
  f$calculate(task)

  p = autoplot(f)
  expect_true(is_ggplot(p))
  expect_doppelganger("filter_1", p)

  p = autoplot(f, n = 2)
  expect_true(is_ggplot(p))
  expect_doppelganger("filter_2", p)
})
