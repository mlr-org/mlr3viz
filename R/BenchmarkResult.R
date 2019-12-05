#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult], depending on argument `type`:
#'
#' * `"boxplot"` (default): Boxplots of performance measures, one box per [mlr3::Learner] and one facet per [mlr3::Task].
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   The predictions of the individual [mlr3::Resampling]s are merged prior to calculating the ROC curve
#'   (micro averaged). Requires package \CRANpkg{precrec}.
#'   If the [mlr3::BenchmarkResult] has multiple tasks, a single task must be selected via argument `task_id`.
#' * `"prc"`: Precision recall curve. See `"roc"`.
#'
#' @param object ([mlr3::BenchmarkResult]).
#' @param type (character(1)):\cr
#'   Type of the plot.
#' @param measure ([mlr3::Measure]).
#' @param task_id (`character(1)`):\cr
#'   Id of the task to operate on.
#' @param ... (`any`):
#'   Currently ignored.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' tasks = tsks(c("spam", "pima", "sonar"))
#' learner = lrns(c("classif.featureless", "classif.rpart"), predict_type = "prob")
#' resampling = rsmps("cv")
#' object = benchmark(benchmark_grid(tasks, learner, resampling))
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object, type = "roc", task_id = "spam")
#' autoplot(object, type = "prc", task_id = "pima")
autoplot.BenchmarkResult = function(object, type = "boxplot", measure = NULL, task_id = NULL, ...) {
  task = object$data$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  tab = fortify(object, measure = measure)

  switch(type,
    "boxplot" = {
      ggplot(object, measure = measure, aes_string("learner_id", measure$id)) +
        geom_boxplot() + xlab("") + facet_wrap("task_id")
    },

    "roc" = {
      autoplot_roc_bmr(object, task_id, "ROC")
    },

    "prc" = {
      autoplot_roc_bmr(object, task_id, "PRC")
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
