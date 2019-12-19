#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult], depending on argument `type`:
#'
#' * `"boxplot"` (default): Boxplots of performance measures, one box per [mlr3::Learner] and one facet per [mlr3::Task].
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   The [mlr3::BenchmarkResult] may only have a single [mlr3::Task] and a single [mlr3::ResampleResult].
#'   Note that you can subset any [mlr3::BenchmarkResult] with its `$filter()` method (see examples).
#'   Requires package \CRANpkg{precrec}.
#' * `"prc"`: Precision recall curve. See `"roc"`.
#'
#' @param object ([mlr3::BenchmarkResult]).
#' @template param_type
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
#' tasks = tsks(c("spam", "pima", "sonar"))
#' learner = lrns(c("classif.featureless", "classif.rpart"), predict_type = "prob")
#' resampling = rsmps("cv")
#' object = benchmark(benchmark_grid(tasks, learner, resampling))
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object$clone()$filter(task_ids = "spam"), type = "roc")
#' autoplot(object$clone()$filter(task_ids = "pima"), type = "prc")
autoplot.BenchmarkResult = function(object, type = "boxplot", measure = NULL, ...) {
  assert_string(type)

  task = object$data$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  tab = fortify(object, measure = measure)

  switch(type,
    "boxplot" = {
      ggplot(object, measure = measure, aes_string("learner_id", measure$id)) +
        geom_boxplot(...) + xlab("") + facet_wrap("task_id")
    },

    "roc" = {
      require_namespaces("precrec")
      autoplot(precrec::evalmod(as_precrec(object)), curvetype = "ROC", show_cb = TRUE)
    },

    "prc" = {
      require_namespaces("precrec")
      autoplot(precrec::evalmod(as_precrec(object)), curvetype = "PRC", show_cb = TRUE)
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
fortify.BenchmarkResult = function(model, data = NULL, measure = NULL, ...) {
  task = model$data$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  model$score(measures = measure)[, c("nr", "task_id", "learner_id", "resampling_id", measure$id), with = FALSE]
}
