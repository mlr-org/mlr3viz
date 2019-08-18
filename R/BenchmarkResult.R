#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult].
#'
#' @param object ([mlr3::BenchmarkResult]).
#' @param measure ([mlr3::Measure]).
#' @param ... (`any`):
#'   Currently ignored.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' tasks = mlr_tasks$mget(c("iris", "pima", "sonar"))
#' learner = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
#' resampling = mlr_resamplings$mget("cv")
#' object = benchmark(benchmark_grid(tasks, learner, resampling))
#'
#' head(fortify(object))
#' autoplot(object)
autoplot.BenchmarkResult = function(object, measure = NULL, ...) {
  measure = mlr3::assert_measure(measure, task = object$data$task[[1L]])
  tab = fortify(object, measure = measure)

  ggplot(object, measure = measure, aes_string("learner_id", measure$id)) +
    geom_boxplot() + facet_wrap("task_id")
}

#' @export
fortify.BenchmarkResult = function(model, data = NULL, measure = NULL, ...) {
  measure = mlr3::assert_measure(measure, task = model$data$task[[1L]])
  model$performance(measures = measure)[, c("nr", "task_id", "learner_id", "resampling_id", measure$id), with = FALSE]
}
