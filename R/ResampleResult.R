#' @title Plot for ResampleResult
#'
#' @description
#' Generates plots for [mlr3::ResampleResult].
#'
#' @param object ([mlr3::ResampleResult]).
#' @param type (character(1)):
#'   Type of the plot.
#' @param measure ([mlr3::Measure]).
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' resampling = mlr_resamplings$get("cv")
#' object = resample(task, learner, resampling)
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object, type = "histogram", binwidth = 0.01)
autoplot.ResampleResult = function(object, type = "boxplot", measure = NULL, ...) {
  task = object$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)

  switch(type,
    "boxplot" =
      ggplot(object, measure = measure, aes_string(y = "performance")) + geom_boxplot(...) + ylab(measure$id),
    "histogram" =
      ggplot(object, measure = measure, aes_string(x = "performance")) + geom_histogram(...) + xlab(measure$id),
    stop("Unknown type")
  )
}

#' @export
fortify.ResampleResult = function(model, data, measure = NULL, ...) {
  task = model$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  data = model$performance(measure)[, c("iteration", measure$id), with = FALSE]
  melt(data, measure.vars = measure$id,
    variable.name = "measure_id", value.name = "performance")
}
