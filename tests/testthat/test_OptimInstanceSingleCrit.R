test_that("fortify.OptimInstanceSingleCrit", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("paradox")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("bbotk")
  library("bbotk") # nolint
  library("paradox") # nolint

  fun = function(xs) {
    c(y = - (xs[[1]] - 2)^2 - (xs[[2]] + 3)^2 + 10)
  }
  domain = ps(
    x1 = p_dbl(-10,10),
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

  f = fortify(instance)
  expect_data_table(f, nrows = 20, ncols = 7)
  expect_names(names(f), permutation.of = c("x1", "x2", "y", "timestamp", "batch_nr",
                                            "x_domain_x1", "x_domain_x2"))
})

test_that("autoplot.OptimInstanceSingleCrit", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("paradox")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("ranger")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("bbotk")
  library("bbotk") # nolint
  library("paradox") # nolint

  fun = function(xs) {
    c(y = - (xs[[1]] - 2)^2 - (xs[[2]] + 3)^2 + 10)
  }
  domain = ps(
    x1 = p_dbl(-10,10),
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

  p = autoplot(instance, type = "marginal")
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "marginal", cols_x = "x_domain_x1")
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "marginal", trafo = TRUE)
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "marginal")
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "performance")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "parameter")
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "parameter", cols_x = "x_domain_x1")
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "parameter", trafo = TRUE)
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "parameter", return_list = TRUE)
  expect_class(p, "DelayedPatchworkPlot")
  lapply(p, function(x) expect_true(is.ggplot(x)))

  p = autoplot(instance, type = "points")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "parallel")
  expect_true(is.ggplot(p))

  p = autoplot(instance, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  # categorical domain is tested through test_TuningInstanceSingleCrit
})
