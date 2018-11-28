#' @title Plot for ResampleResult
#'
#' @description
#' Generates plots for [mlr3::ResampleResult].
#'
#' @param object \[[mlr3::ResampleResult]\].
#' @param type \[character(1)\]: Type of the plot.
#' @param measure \[[mlr3::Measure]\]. If NULL, defaults to first measure, i.e.
#'  `object$measures[[1]]`.
#' @param ... \[any\]: Additional arguments, passed down to the respective `geom`.
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
#' library(ggplot2)
#' autoplot(object)
#' autoplot(object, type = "histogram", binwidth = 0.01)
autoplot.ResampleResult = function(object, type = "boxplot", measure = NULL, ...) {
  measure = assert_measure(measure %??% object$measures[[1L]])

  switch(type,
    "boxplot" =
      ggplot(object, measure = measure, aes_string(x = "measure_id", y = "performance")) + geom_boxplot(...),
    "histogram" =
      ggplot(object, measure = measure, aes_string(x = "performance")) + geom_histogram(...) + xlab(measure$id),
    stop("Unknown type")
  )
}

#' @export
fortify.ResampleResult = function(model, data, measure = NULL, ...) {
  measure = assert_measure(measure %??% model$measures$measure[[1L]])

  data = unnest(model$data[, c("iteration", "performance")], "performance")
  melt(data, measure.vars = ids(model$measures),
    variable.name = "measure_id", value.name = "performance")[get("measure_id") == measure$id]
}
