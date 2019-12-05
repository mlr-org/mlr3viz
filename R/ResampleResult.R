#' @title Plot for ResampleResult
#'
#' @description
#' Generates plots for [mlr3::ResampleResult], depending on argument `type`:
#' * `"boxplot"` (default): Boxplot of performance measures.
#' * `"histogram"`: Histogram of performance measures.
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   The predictions of the individual [mlr3::Resampling]s are merged prior to calculating the ROC curve
#'   (micro averaged). Requires package \CRANpkg{precrec}.
#' * `"prc"`: Precision recall curve. See `"roc"`.
#'
#' @param object ([mlr3::ResampleResult]).
#' @param type (character(1)):\cr
#'   Type of the plot.
#' @param measure ([mlr3::Measure]).
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("sonar")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' resampling = rsmp("cv")
#' object = resample(task, learner, resampling)
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object, type = "histogram")
#' autoplot(object, type = "roc")
#' autoplot(object, type = "prc")
autoplot.ResampleResult = function(object, type = "boxplot", measure = NULL, ...) {
  task = object$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)

  switch(type,
    "boxplot" = {
      ggplot(object, measure = measure, aes_string(y = "performance")) + geom_boxplot(...) + ylab(measure$id)
    },

    "histogram" = {
      ggplot(object, measure = measure, aes_string(x = "performance")) + geom_histogram(...) + xlab(measure$id)
    },

    "roc" = {
      autoplot(object$prediction(), type = "roc")
    },

    "prc" = {
      autoplot(object$prediction(), type = "prc")
    },

    stop("Unknown type")
  )
}

#' @export
fortify.ResampleResult = function(model, data, measure = NULL, ...) {
  task = model$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  data = model$score(measure)[, c("iteration", measure$id), with = FALSE]
  melt(data, measure.vars = measure$id,
    variable.name = "measure_id", value.name = "performance")
}
