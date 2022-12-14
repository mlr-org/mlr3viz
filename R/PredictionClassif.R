#' @title Plot for PredictionClassif
#'
#' @description
#' Generates plots for [mlr3::PredictionClassif], depending on argument `type`:
#'
#' * `"stacked"` (default): Stacked barplot of true and estimated class labels.
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   Requires package \CRANpkg{precrec}.
#' * `"prc"`: Precision recall curve.
#'   Requires package \CRANpkg{precrec}.
#' * `"threshold"`: Systematically varies the threshold of the [mlr3::PredictionClassif]
#'   object and plots the resulting performance as returned by `measure`.
#'   Arguments in `...` are passed down to the score function of the [mlr3::Measure].
#'
#' @param object ([mlr3::PredictionClassif]).
#' @template param_type
#' @template param_measure
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`, plotting function or measure.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
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
#'   task = tsk("spam")
#'   learner = lrn("classif.rpart", predict_type = "prob")
#'   object = learner$train(task)$predict(task)
#'
#'   head(fortify(object))
#'   autoplot(object)
#'   autoplot(object, type = "roc")
#'   autoplot(object, type = "prc")
#' }
autoplot.PredictionClassif = function(object, type = "stacked", measure = NULL, ...) { # nolint
  assert_string(type)

  switch(type,
    "stacked" = {
      tab = melt(fortify(object)[, c("truth", "response")], measure.vars = c("truth", "response"))
      ggplot(tab,
        mapping = aes(
          fill = .data[["value"]],
          x = .data[["variable"]])) +
        geom_bar(width = 0.5, color = "#000000", ...) +
        geom_label(
          mapping = aes(label = after_stat(count)),
          stat = "count",
          position = position_stack(vjust = 0.5), colour = "white") +
        xlab("Feature") +
        ylab("Count") +
        apply_theme(list(
          scale_fill_viridis_d("Feature", end = 0.8),
          theme_mlr3()))
    },

    "roc" = {
      plot_precrec(object, curvetype = "ROC", ...) +
        apply_theme(list(
          scale_color_viridis_d(begin = 0.5, guide = "none"),
          theme_mlr3(legend = "none"),
          theme(plot.title = element_blank())
        ))

    },

    "prc" = {
      plot_precrec(object, curvetype = "PRC", ...) +
        apply_theme(list(
          scale_color_viridis_d(begin = 0.5, guide = "none"),
          theme_mlr3(legend = "none"),
          theme(plot.title = element_blank())
        ))
    },

    "threshold" = {
      measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = object$task_type))
      pred = object$clone(deep = TRUE)
      tab = data.table(prob = seq(from = 0, to = 1, by = 0.01))
      tab$score = map_dbl(tab$prob, function(p) pred$set_threshold(p)$score(measure, ...))
      ggplot(tab,
        mapping = aes(
          x = .data[["prob"]],
          y = .data[["score"]])) +
        geom_line(color = apply_theme(viridis::viridis(1, begin = 0.5), "#3366FF")) +
        xlab("Probability Threshold") +
        ylab(measure$id) +
        apply_theme(list(
          scale_color_viridis_d(),
          theme_mlr3()
        ))
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.PredictionClassif = function(x, ...) {
  print(autoplot(x, ...))
}
