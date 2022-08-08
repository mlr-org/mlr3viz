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

test_that("autoplot.PipeOpFilter", {
  task = mlr3::tsk("spam")
  pop = mlr3pipelines::po("filter", mlr3filters::flt("auc"), filter.frac = 0.5)
  pop$train(list(task))
  p = autoplot(pop)
  expect_true(is.ggplot(p))
})
