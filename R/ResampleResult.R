#' @title Plot for ResampleResult
#'
#' @description
#' Generates plots for [mlr3::ResampleResult].
#'
#' @param object \[[mlr3::ResampleResult]\].
#' @param type \[character(1)\]: Type of the plot.
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
autoplot.ResampleResult = function(object, type = "boxplot", ...) {
  switch(type,
    "boxplot" =
      ggplot(object, aes_string(x = "measure", y = "performance")) + geom_boxplot(...),
    "histogram" =
      ggplot(object, aes_string(x = "performance")) + geom_histogram(...),
    stop("Unknown type")
  )
}

#' @export
fortify.ResampleResult = function(model, data, ...) {
  data = unnest(model$data[, c("iteration", "performance")], "performance")
  melt(data, measure.vars = ids(model$measures),
    variable.name = "measure", value.name = "performance")
}
