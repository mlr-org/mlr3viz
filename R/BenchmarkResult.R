#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult], depending on argument `type`:
#'
#' * `"boxplot"` (default): Boxplots of performance measures, one box per
#' [mlr3::Learner] and one facet per [mlr3::Task].
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   The [mlr3::BenchmarkResult] may only have a single [mlr3::Task] and a
#'   single [mlr3::Resampling].
#'   Note that you can subset any [mlr3::BenchmarkResult] with its `$filter()`
#'   method (see examples).
#'   Requires package \CRANpkg{precrec}.
#'   Additional arguments will be passed down to the respective [autoplot()] function
#'   in package \CRANpkg{precrec}. Arguments `calc_avg` and `cb_alpha` are passed to
#'   [precrec::evalmod()].
#' * `"prc"`: Precision recall curve. See `"roc"`.
#'
#' @param object ([mlr3::BenchmarkResult]).
#' @template param_type
#' @template param_measure
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom` or plotting function.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @references
#' `r format_bib("precrec")`
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' tasks = tsks(c("pima", "sonar"))
#' learner = lrns(c("classif.featureless", "classif.rpart"),
#'   predict_type = "prob")
#' resampling = rsmps("cv")
#' object = benchmark(benchmark_grid(tasks, learner, resampling))
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object$clone(deep = TRUE)$filter(task_ids = "pima"), type = "roc")
autoplot.BenchmarkResult = function(object, # nolint
  type = "boxplot",
  measure = NULL,
  ...) {

  assert_string(type)

  task = object$tasks$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure,
    task_type = task$task_type), task = task)
  measure_id = measure$id
  tab = fortify(object, measure = measure)
  tab$nr = sprintf("%09d", tab$nr)
  learner_label_map = tab[!duplicated(tab$nr), c("nr", "learner_id")]
  learner_labels = learner_label_map$learner_id
  names(learner_labels) = learner_label_map$nr

  switch(type,
    "boxplot" = {
      ggplot(tab, mapping = aes(x = .data$nr, y = .data[[measure_id]])) +
        geom_boxplot(...) +
        labs(x = "") +
        scale_x_discrete(labels = learner_labels) +
        # we need "free_x" to drop empty learners for certain tasks - because
        # we apply over .data$nr
        facet_wrap(vars(.data$task_id), scales = "free_x")
    },

    "roc" = {
      plot_precrec(object, curvetype = "ROC", ...)
    },

    "prc" = {
      plot_precrec(object, curvetype = "PRC", ...)
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.BenchmarkResult = function(x, ...) {
  print(autoplot(x, ...))
}

#' @export
fortify.BenchmarkResult = function(model, data = NULL, measure = NULL, ...) { # nolint
  task = model$tasks$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure,
    task_type = task$task_type), task = task)
  model$score(measures = measure)[, c(
    "nr", "task_id", "learner_id",
    "resampling_id", measure$id), with = FALSE]
}
