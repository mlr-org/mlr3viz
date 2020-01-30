#' @title Plot for PredictionRegr
#'
#' @description
#' Generates plots for [mlr3::PredictionRegr], depending on argument `type`:
#'
#' * `"xy"` (default): Scatterplot of true response vs predicted response.
#'   Additionally fits a linear model to visualize a possible trend.
#' * `"histogram"`: Histogram of residuals \eqn{r = y - \hat{y}}{r = y - y.hat}.
#' * `"residual"`: Plot of the residuals, with the response \eqn{\hat{y}}{y.hat} on the "x" and the residuals on the "y" axis.
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
autoplot.PredictionRegr = function(object, type = "xy", ...) {
  assert_string(type)

  switch(type,
    "xy" = {
      ggplot(object, aes_string(x = "response", y = "truth")) + geom_abline(slope = 1, alpha = 0.5) + geom_point(...) + geom_rug(sides = "bl") + geom_smooth(method = "lm")
    },

    "histogram" = {
      object = fortify(object)
      ggplot(object, aes_string(x = "truth-response", y = "..density..")) + geom_histogram(...)
    },

    "residual" = {
      ggplot(object, aes_string(x = "response", y = "truth-response")) + geom_point(...) + geom_rug(sides = "bl") + geom_smooth(method = "lm")
    },

    stopf("Unknown plot type '%s'", type)
  )
}
