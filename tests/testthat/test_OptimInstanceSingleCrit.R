skip_if_not_installed("mlr3")
skip_if_not_installed("bbotk")
skip_if_not_installed("patchwork")
library("bbotk") # nolint
requireNamespace("mlr3learners")
set.seed(42)

fun = function(xs) {
  c(y = -(xs[[1]] - 2)^2 - (xs[[2]] + 3)^2 + 10)
}
domain = ps(
  x1 = p_dbl(-10, 10),
  x2 = p_dbl(-5, 5)
)
codomain = ps(
  y = p_dbl(tags = "maximize")
)
obfun = ObjectiveRFun$new(
  fun = fun,
  domain = domain,
  codomain = codomain
)

instance = OptimInstanceSingleCrit$new(objective = obfun, terminator = trm("evals", n_evals = 20))

optimizer = opt("random_search", batch_size = 2)
optimizer$optimize(instance)

test_that("fortify.OptimInstanceSingleCrit", {
  f = fortify(instance)
  expect_data_table(f, nrows = 20, ncols = 7)
  expect_names(names(f), permutation.of = c("x1", "x2", "y", "timestamp", "batch_nr", "x_domain_x1", "x_domain_x2"))
})

test_that("autoplot.OptimInstanceSingleCrit", {
  expect_single = function(id, plot) {
    expect_true(is.ggplot(plot))
    vdiffr::expect_doppelganger(sprintf("tisc_%s", id), plot)
  }

  expect_multiple = function(id, plots) {
    assert_string(id)
    expect_class(plots, "DelayedPatchworkPlot")
    for (i in seq_along(plots)) {
      cur = plots[[i]]
      expect_true(is.ggplot(cur))
      vdiffr::expect_doppelganger(sprintf("tisc_%s_%02i", id, i), cur)
    }
  }

  p = autoplot(instance, type = "marginal")
  expect_multiple("marginal", p)

  p = autoplot(instance, type = "marginal", cols_x = "x_domain_x1")
  expect_multiple("marginal_x_domain", p)

  p = autoplot(instance, type = "marginal", trafo = TRUE)
  expect_multiple("marginal_trafo", p)

  p = autoplot(instance, type = "performance")
  expect_single("performance", p)

  p = autoplot(instance, type = "parameter")
  expect_multiple("parameter", p)

  p = autoplot(instance, type = "parameter", cols_x = "x_domain_x1")
  expect_multiple("parameter_x_domain", p)

  p = autoplot(instance, type = "parameter", trafo = TRUE)
  expect_multiple("parameter_trafo", p)

  p = autoplot(instance, type = "parameter", return_list = TRUE)
  expect_multiple("parameter_return_list", p)

  p = autoplot(instance, type = "surface")
  expect_single("surface", p)

  p = autoplot(instance, type = "surface", grid_resolution = 50)
  expect_single("surface_grid_50", p)

  p = autoplot(instance, type = "surface", learner = mlr3::lrn("regr.lm"))
  expect_single("surface_regr_lm", p)

  p = autoplot(instance, type = "points")
  expect_single("points", p)

  p = autoplot(instance, type = "parallel")
  expect_single("parallel", p)

  p = autoplot(instance, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  # categorical domain is tested through test_TuningInstanceSingleCrit
})
