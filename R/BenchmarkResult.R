#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult], depending on argument `type`:
#'
#' * `"boxplot"` (default): Boxplots of performance measures, one box per [mlr3::Learner] and one facet per [mlr3::Task].
#'
#' @param object ([mlr3::BenchmarkResult]).
#' @param type (character(1)):\cr
#'   Type of the plot.
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
autoplot.BenchmarkResult = function(object, type = "boxplot", measure = NULL, ...) {
  task = object$data$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  tab = fortify(object, measure = measure)

  switch(type,
    "boxplot" = {
      ggplot(object, measure = measure, aes_string("learner_id", measure$id)) +
        geom_boxplot() + facet_wrap("task_id")
    },

    stop("Unknown type")
  )
}

#' @export
fortify.BenchmarkResult = function(model, data = NULL, measure = NULL, ...) {
  task = model$data$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  model$score(measures = measure)[, c("nr", "task_id", "learner_id", "resampling_id", measure$id), with = FALSE]
}
