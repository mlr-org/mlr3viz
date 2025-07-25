#' @title Plots for Benchmark Results
#'
#' @description
#' Visualizations for [mlr3::BenchmarkResult].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"boxplot"` (default): Boxplots of performance measures, one box per [mlr3::Learner] and one facet per [mlr3::Task].
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   The [mlr3::BenchmarkResult] may only have a single [mlr3::Task] and a single [mlr3::Resampling].
#'   Note that you can subset any [mlr3::BenchmarkResult] with its `$filter()`  method (see examples).
#'   Requires package \CRANpkg{precrec}.
#' * `"prc"`: Precision recall curve.
#'    See `"roc"`.
#' * `"ci"`: Plot confidence intervals. Pass a `msr("ci", ...)` from the `mlr3inferr` package as argument `measure`.
#'
#' @param object ([mlr3::BenchmarkResult]).
#' @template param_type
#' @template param_measure
#' @template param_theme
#' @param ... arguments passed on to [precrec::autoplot()] for `type = "roc"` or `"prc"`.
#' Useful to e.g. remove confidence bands with `show_cb = FALSE`.
#'
#' @return [ggplot2::ggplot()].
#'
#' @references
#' `r format_bib("precrec")`
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3viz)
#'
#'   tasks = tsks(c("pima", "sonar"))
#'   learner = lrns(c("classif.featureless", "classif.rpart"),
#'     predict_type = "prob")
#'   resampling = rsmps("cv")
#'   object = benchmark(benchmark_grid(tasks, learner, resampling))
#'
#'   head(fortify(object))
#'   autoplot(object)
#'   autoplot(object$clone(deep = TRUE)$filter(task_ids = "pima"), type = "roc")
#' }
autoplot.BenchmarkResult = function(object, type = "boxplot", measure = NULL, theme = theme_minimal(), ...) {
  assert_choice(type, choices = c("boxplot", "roc", "prc", "ci"), null.ok = FALSE)

  task = object$tasks$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)

  if (identical(type, "ci")) {
    mlr3misc::require_namespaces("mlr3inferr")

    assert_class(measure, "MeasureAbstractCi")
    mid = measure$id

    tbl = object$aggregate(measure)

    tmp = map(object$resamplings$resampling, function(x) {
      list(class(x), x$param_set$values)
    })

    if (length(unique(tmp)) != 1) {
      stopf("Plot of type 'ci' requires exactly one resampling method")
    }

    # static checker
    .data = NULL
    task_id = NULL
    p = ggplot(tbl, aes(x = .data[["learner_id"]], y = .data[[mid]])) +
      geom_point() +
      geom_errorbar(aes(ymin = .data[[paste0(mid, ".lower")]], ymax = .data[[paste0(mid, ".upper")]]), width = 0.2) +
      facet_wrap(vars(task_id), scales = "free_y") +
      labs(
      title = sprintf("Confidence Intervals for alpha = %s", measure$param_set$values$alpha),
      x = "Learner",
      y = paste0(measure$measure$id)
      ) +
      theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()
      )
    return(p)
  }

  measure_id = measure$id
  tab = fortify(object, measure = measure)
  tab$nr = sprintf("%09d", tab$nr)
  learner_label_map = tab[!duplicated(tab$nr), c("nr", "learner_id")]
  learner_labels = learner_label_map$learner_id
  names(learner_labels) = learner_label_map$nr

  switch(type,
    "boxplot" = {
      ggplot(tab,
        mapping = aes(
          x = .data$nr,
          y = .data[[measure_id]])) +
        geom_boxplot(
          mapping = aes(fill = .data[["learner_id"]]),
          show.legend = FALSE) +
        scale_x_discrete(labels = learner_labels) +
        # we need "free_x" to drop empty learners for certain tasks - because we apply over .data$nr
        facet_wrap(vars(.data$task_id), scales = "free_x") +
        scale_fill_viridis_d("Learner", end = 0.8, alpha = 0.8) +
        theme +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank()
        )
    },

    "roc" = {
      p = plot_precrec(object, curvetype = "ROC", ...)
      p$layers[[1]]$mapping = aes(color = modname, fill = modname)
      # fill confidence bounds
      p +
        scale_color_viridis_d("Learner", end = 0.8, aesthetics = c("color", "fill")) +
        theme +
        labs(title = NULL)
    },

    "prc" = {
      p = plot_precrec(object, curvetype = "PRC", ...)
      # fill confidence bounds
      p$layers[[1]]$mapping = aes(color = modname, fill = modname)
      p +
        scale_color_viridis_d("Learner", end = 0.8, aesthetics = c("color", "fill")) +
        theme +
        labs(title = NULL)
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
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  model$score(measures = measure)[, c("nr", "task_id", "learner_id", "resampling_id", measure$id), with = FALSE]
}
