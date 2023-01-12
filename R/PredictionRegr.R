#' @title Plots for Regression Predictions
#'
#' @description
#' Visualizations for [mlr3::PredictionRegr].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"xy"` (default): Scatterplot of "true" response vs. "predicted" response.
#'    By default a linear model is fitted via `geom_smooth(method = "lm")` to visualize the trend between x and y (by default colored blue).
#'    In addition `geom_abline()` with `slope = 1` is added to the plot.
#'    Note that `geom_smooth()` and `geom_abline()` may overlap, depending on the given data.
#' * `"histogram"`: Histogram of residuals: \eqn{r = y - \hat{y}}{r = y - y.hat}.
#' * `"residual"`: Plot of the residuals, with the response \eqn{\hat{y}}{y.hat} on the "x" and the residuals on the "y" axis.
#'    By default a linear model is fitted via `geom_smooth(method = "lm")` to visualize the trend between x and y (by default colored blue).
#'
#' @param object ([mlr3::PredictionRegr]).
#' @template param_type
#' @param binwidth (`integer(1)`)\cr
#'  Width of the bins for the histogram.
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3viz)
#'
#'   task = tsk("boston_housing")
#'   learner = lrn("regr.rpart")
#'   object = learner$train(task)$predict(task)
#'
#'   head(fortify(object))
#'   autoplot(object)
#'   autoplot(object, type = "histogram", binwidth = 1)
#'   autoplot(object, type = "residual")
#' }
autoplot.PredictionRegr = function(object, type = "xy", binwidth = NULL, theme = theme_minimal(), ...) {
  checkmate::assert_string(type)

  switch(type,
    "xy" = {
      ggplot(object,
        mapping = aes(
          x = .data[["response"]],
          y = .data[["truth"]])) +
        geom_abline(
          slope = 1,
          alpha = 0.5) +
        geom_point(
          color = viridis::viridis(1, begin = 0.33),
          alpha = 0.8) +
        geom_rug(sides = "bl") +
        geom_smooth(
          formula = y ~ x,
          method = "lm",
          color = viridis::viridis(1, begin = 0.5)) +
        theme
    },

    "histogram" = {
      object = ggplot2::fortify(object)
      ggplot(object,
        mapping = aes(
          x = .data[["truth"]] - .data[["response"]],
          y = after_stat(.data[["density"]]))) +
        geom_histogram(
          fill = viridis::viridis(1, begin = 0.5),
          alpha = 0.8,
          color = "black",
          binwidth = binwidth) +
        xlab("Residuals") +
        ylab("Density") +
        theme
    },

    "residual" = {
      ggplot(object,
        mapping = aes(
          x = .data[["response"]],
          y = .data[["truth"]] - .data[["response"]])) +
        geom_point(
          color = viridis::viridis(1, begin = 0.33),
          alpha = 0.8) +
        geom_rug(sides = "bl") +
        geom_smooth(
          formula = y ~ x,
          method = "lm",
          fill = viridis::viridis(1, begin = 0.5),
          color = viridis::viridis(1, begin = 0.5)) +
        xlab("Response") +
        ylab("Residuals") +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.PredictionRegr = function(x, ...) {
  print(autoplot(x, ...))
}
