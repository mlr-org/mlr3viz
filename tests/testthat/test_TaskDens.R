test_that("autoplot.TaskDens", {
  skip_if_not_installed("mlr3proba")
  require_namespaces("mlr3proba")
  task = mlr3::tsk("precip")

  p = autoplot(task, type = "dens")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_dens", p)

  p = autoplot(task, type = "freq")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_freq", p)

  p = autoplot(task, type = "overlay")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_overlay", p)

  p = autoplot(task, type = "freqpoly")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_freqpoly", p)
})
