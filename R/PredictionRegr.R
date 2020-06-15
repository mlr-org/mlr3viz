#' @title Plot for PredictionRegr
#'
#' @importFrom ggplot2 fortify
#' @description
#' Generates plots for [mlr3::PredictionRegr], depending on argument `type`:
#'
#' * `"xy"` (default): Scatterplot of "true" response vs. "predicted" response.
#'   By default a linear model is fitted via `geom_smooth(method = "lm")`
#'   to visualize the trend between x and y (by default colored blue).
#'
#'   * In addition `geom_abline()` with `slope = 1` is added to the plot.
#'
#'   * Note that `geom_smooth()` and `geom_abline()` may overlap, depending on
#'     the given data.
#' * `"histogram"`: Histogram of residuals:
#'    \eqn{r = y - \hat{y}}{r = y - y.hat}.
#' * `"residual"`: Plot of the residuals, with the response \eqn{\hat{y}}{y.hat}
#' on the "x" and the residuals on the "y" axis.
#'
#'   * By default a linear model is fitted via `geom_smooth(method = "lm")`
#'   to visualize the trend between x and y (by default colored blue).
#'
#' @param object ([mlr3::PredictionRegr]).
#' @template param_type
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("boston_housing")
#' learner = lrn("regr.rpart")
#' object = learner$train(task)$predict(task)
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object, type = "histogram", binwidth = 1)
#' autoplot(object, type = "residual")
autoplot.PredictionRegr = function(object, # nolint
  type = "xy",
  ...) {
  checkmate::assert_string(type)

  switch(type,
    "xy" = {
      ggplot(object,
        mapping = aes(x = .data[["response"]], y = .data[["truth"]])
      ) +
        geom_abline(slope = 1, alpha = 0.5) +
        geom_point(...) +
        geom_rug(sides = "bl") +
        geom_smooth(method = "lm")
    },

    "histogram" = {
      object = ggplot2::fortify(object)
      ggplot(object,
        mapping = aes(
          x = .data[["truth"]] - .data[["response"]],
          y = after_stat(.data[["density"]]))
      ) +
        geom_histogram(...)
    },

    "residual" = {
      ggplot(object,
        mapping = aes(
          x = .data[["response"]],
          y = .data[["truth"]] - .data[["response"]])
      ) +
        geom_point(...) +
        geom_rug(sides = "bl") +
        geom_smooth(method = "lm")
    },

    stopf("Unknown plot type '%s'", type)
  )
}
