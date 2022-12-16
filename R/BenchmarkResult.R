#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult], depending on argument `type`:
#'
#' * `"boxplot"` (default): Boxplots of performance measures, one box per [mlr3::Learner] and one facet per [mlr3::Task].
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   The [mlr3::BenchmarkResult] may only have a single [mlr3::Task] and a single [mlr3::Resampling].
#'   Note that you can subset any [mlr3::BenchmarkResult] with its `$filter()`  method (see examples).
#'   Requires package \CRANpkg{precrec}.
#'   Additional arguments will be passed down to the respective [autoplot()] function in package \CRANpkg{precrec}.
#'   Arguments `calc_avg` and `cb_alpha` are passed to [precrec::evalmod()].
#' * `"prc"`: Precision recall curve. See `"roc"`.
#'
#' @param object ([mlr3::BenchmarkResult]).
#' @template param_type
#' @template param_measure
#' @template style
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @section Style:
#' The following arguments can be set in `style`:
#'
#' * `boxplot_boxplot`: List of arguments passed to [ggplot2::geom_boxplot()].
#' * `boxplot_scales`: List of scales.
#' * `theme_boxplot`: List of themes.
#' * `roc_precrec`: List of arguments passed to [precrec::plot_precrec()].
#' * `roc_scales`: List of scales.
#' * `roc_themes`: List of themes.
#' * `prc_precrec`: List of arguments passed to [precrec::plot_precrec()].
#' * `prc_scales`: List of scales.
#' * `prc_themes`: List of themes.
#'
#'
#' The following default style is set:
#'
#' ```{r}
#' default_style = list(
#'    boxplot_boxplot = list(show.legend = FALSE),
#'    boxplot_scales = list(scale_fill_viridis_d("Learner", end = 0.8, alpha = 0.8)),
#'    theme_boxplot = list(
#'      theme_minimal(),
#'      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())),
#'    roc_precrec = list(),
#'    roc_scales = list(scale_color_viridis_d("Learner", end = 0.8, aesthetics = c("color", "fill"))),
#'    roc_themes = list(theme_minimal(), theme(plot.title = element_blank())),
#'    prc_precrec = list(),
#'    prc_scales = list(scale_color_viridis_d("Learner", end = 0.8, aesthetics = c("color", "fill"))),
#'    prc_themes = list(theme_minimal(), theme(plot.title = element_blank()))
#'  )
#'  ```
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
autoplot.BenchmarkResult = function(object, type = "boxplot", measure = NULL, style = named_list()) {
  assert_string(type)

  default_style = list(
    boxplot_boxplot = list(show.legend = FALSE),
    boxplot_scales = list(scale_fill_viridis_d("Learner", end = 0.8, alpha = 0.8)),
    theme_boxplot = list(
      theme_minimal(),
      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())),
    roc_precrec = list(),
    roc_scales = list(scale_color_viridis_d("Learner", end = 0.8, aesthetics = c("color", "fill"))),
    roc_themes = list(theme_minimal(), theme(plot.title = element_blank())),
    prc_precrec = list(),
    prc_scales = list(scale_color_viridis_d("Learner", end = 0.8, aesthetics = c("color", "fill"))),
    prc_themes = list(theme_minimal(), theme(plot.title = element_blank()))
  )
  style = apply_style(style, default_style)

  task = object$tasks$task[[1L]]
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
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
        invoke(geom_boxplot,
          mapping = aes(fill = .data[["learner_id"]]),
          .args = style$boxplot) +
        scale_x_discrete(labels = learner_labels) +
        # we need "free_x" to drop empty learners for certain tasks - because we apply over .data$nr
        facet_wrap(vars(.data$task_id), scales = "free_x") +
        style$boxplot_scales +
        style$theme_boxplot
    },

    "roc" = {
      p = invoke(plot_precrec, object, curvetype = "ROC", .args = style$roc_precrec)
      # fill confidence bounds
      p$layers[[1]]$mapping = aes(colour = modname, fill = modname)

      p +
        style$roc_scales +
        style$roc_themes
    },

    "prc" = {
      p = invoke(plot_precrec, object, curvetype = "PRC", .args = style$prc_precrec)
      # fill confidence bounds
      p$layers[[1]]$mapping = aes(colour = modname, fill = modname)

      p +
        style$prc_scales +
        style$prc_themes
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
