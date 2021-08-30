test_that("autoplot.Filter", {
  task = mlr3::tsk("iris")
  f = mlr3filters::mlr_filters$get("anova")
  f$calculate(task)

  p = autoplot(f)
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("filter_1", p)

  p = autoplot(f, n = 2)
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("filter_2", p)
})
