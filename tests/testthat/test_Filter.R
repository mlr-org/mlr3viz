context("Filter")

test_that("autoplot.Filter", {
  task = mlr_tasks$get("iris")
  f = mlr3filters::mlr_filters$get("anova")
  f$calculate(task)

  p = autoplot(f)
  expect_true(is.ggplot(p))

  p = autoplot(f, n = 2)
  expect_true(is.ggplot(p))
})
