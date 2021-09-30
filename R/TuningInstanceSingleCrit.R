#' @title Plot for TuningInstanceSingleCrit
#'
#' @description
#' Generates plots for [mlr3tuning::TuningInstanceSingleCrit].
#'
#' @param object ([mlr3tuning::TuningInstanceSingleCrit].
#' @param type (`character(1)`):
#'   Type of the plot. Available choices:
#'   * `"marginal"`: scatter plots of hyperparameter versus performance. The
#'     color of the points shows the batch number.
#'   * `"performance"`: scatter plots of batch number versus performance.
#'   * `"parameter"`: scatter plots of batch number versus hyperparameter. The
#'     color of the points shows the performance.
#'   * `"parallel"` parallel coordinates plot. Parameter values are rescaled by
#'     `(x - mean(x)) / sd(x)`.
#'   * `"points"` - scatter plot of two hyperparameters versus performance. The
#'     color of the points shows the performance.
#'   * `"surface"`: surface plot of 2 hyperparameters versus performance.
#'     The performance values are interpolated with the supplied
#'     [mlr3::Learner].
#'   * `"pairs"`: plots all hyperparameters and performance values against each other.
#' @param cols_x (`character()`)\cr
#'   Column names of hyperparameters. By default, all untransformed
#'   hyperparameters are plotted. Transformed hyperparameters are prefixed with
#'   `x_domain_`.
#' @param trafo (`logical(1)`)\cr
#'   Determines if untransformed (`FALSE`) or transformed (`TRUE`)
#'   hyperparametery are plotted.
#' @param learner ([mlr3::Learner])\cr
#'   Regression learner used to interpolate the data of the surface plot.
#' @param grid_resolution (`numeric()`)\cr
#'   Resolution of the surface plot.
#' @param ... (`any`):
#'   Additional arguments, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' if (requireNamespace("mlr3tuning") && requireNamespace("patchwork")) {
#'   library(mlr3tuning)
#'
#'   learner = lrn("classif.rpart")
#'   learner$param_set$values$cp = to_tune(0.001, 0.1)
#'   learner$param_set$values$minsplit = to_tune(1, 10)
#'
#'   instance = TuningInstanceSingleCrit$new(
#'     task = tsk("iris"),
#'     learner = learner,
#'     resampling = rsmp("holdout"),
#'     measure = msr("classif.ce"),
#'     terminator = trm("evals", n_evals = 10))
#'
#'   tuner = tnr("random_search")
#'
#'   tuner$optimize(instance)
#'
#'   # plot performance versus batch number
#'   autoplot(instance, type = "performance")
#'
#'   # plot cp values versus performance
#'   autoplot(instance, type = "marginal", cols_x = "cp")
#'
#'   # plot transformed parameter values versus batch number
#'   autoplot(instance, type = "parameter", trafo = TRUE)
#'
#'   # plot parallel coordinates plot
#'   autoplot(instance, type = "parallel")
#'
#'   # plot pairs
#'   autoplot(instance, type = "pairs")
#' }
autoplot.TuningInstanceSingleCrit = function(object, type = "marginal", cols_x = NULL, trafo = FALSE,
                                             learner = mlr3::lrn("regr.ranger"), grid_resolution = 100, ...) { # nolint
  autoplot.OptimInstanceSingleCrit(object = object, type = type, cols_x = cols_x, trafo = trafo, learner = learner, grid_resolution = grid_resolution, ...)
}

