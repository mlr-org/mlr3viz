#' @title Plot for ResampleResult
#'
#' @description
#' Generates plots for [mlr3::ResampleResult].
#'
#' @param object ([mlr3::ResampleResult]).
#' @param type (character(1)): Type of the plot.
#' @param measure ([mlr3::Measure]). If NULL, defaults to first measure, i.e.
#'  `object$measures[[1]]`.
#' @param ... : Additional arguments, passed down to the respective `geom`.
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
#' fortify(object)
#' autoplot(object)
#' autoplot(object, type = "histogram", binwidth = 0.01)
autoplot.ResampleResult = function(object, type = "boxplot", measure = NULL, ...) {
  measure = assert_measure(measure %??% object$measures$measure[[1L]])

  switch(type,
    "boxplot" =
      ggplot(object, measure = measure, aes_string(y = measure$id)) + geom_boxplot(...),
    "histogram" =
      ggplot(object, measure = measure, aes_string(x = measure$id)) + geom_histogram(...),
    stop("Unknown type")
  )
}

#' @export
fortify.ResampleResult = function(model, data, measure = NULL, ...) {
  measure = assert_measure(measure %??% model$measures$measure[[1L]])

  unnest(model$data[, c("iteration", "performance"), with = FALSE], "performance")[, c("iteration", measure$id), with = FALSE]
}
