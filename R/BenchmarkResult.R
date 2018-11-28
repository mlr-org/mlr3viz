#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult].
#'
#' @param object \[[mlr3::BenchmarkResult]\].
#' @param measure \[[mlr3::Measure]\]. If NULL, defaults to first measure, i.e.
#'  `object$measures$measure[[1]]`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' tasks = mlr_tasks$mget(c("iris", "pima", "sonar"))
#' learner = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
#' resampling = mlr_resamplings$mget("cv")
#' object = benchmark(tasks, learner, resampling)
#'
#' library(ggplot2)
#' fortify(object)
#' autoplot(object)
autoplot.BenchmarkResult = function(object, measure = NULL) {
  measure = assert_measure(measure %??% object$measures$measure[[1L]])

  ggplot(object, measure = measure, aes_string("learner_id", "performance")) +
    geom_boxplot() + facet_wrap("task_id") + ylab(measure$id)
}

#' @export
fortify.BenchmarkResult = function(model, data = NULL, measure = NULL) {
  measure = assert_measure(measure %??% model$measures$measure[[1L]])

  data = as.data.table(model)[, c("hash", "task_id", "learner_id", "resampling_id", measure$id), with = FALSE]
  melt(data, measure.vars = measure$id,
    variable.name = "measure_id", value.name = "performance")[get("measure_id") == measure$id]
}
