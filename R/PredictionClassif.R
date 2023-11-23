#' @title Plots for Classification Predictions
#'
#' @description
#' Visualizations for [mlr3::PredictionClassif].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"stacked"` (default): Stacked barplot of true and estimated class labels.
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   Requires package \CRANpkg{precrec}.
#' * `"prc"`: Precision recall curve.
#'   Requires package \CRANpkg{precrec}.
#' * `"threshold"`: Systematically varies the threshold of the [mlr3::PredictionClassif] object and plots the resulting performance as returned by `measure`.
#'
#' @param object ([mlr3::PredictionClassif]).
#' @template param_type
#' @template param_measure
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#'
#' @references
#' `r format_bib("precrec")`
#'
#' @export
#' @examples
#' \donttest{
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
#' }
autoplot.PredictionClassif = function(object, type = "stacked", measure = NULL, theme = theme_minimal(), ...) { # nolint
  assert_string(type)

  switch(type,
    "stacked" = {
      tab = melt(fortify(object)[, c("truth", "response")], measure.vars = c("truth", "response"))
      ggplot(tab,
        mapping = aes(
          fill = .data[["value"]],
          x = .data[["variable"]])) +
        geom_bar(
          width = 0.5,
          color = "#000000",
          alpha = 0.8) +
        geom_text(
          mapping = aes(label = after_stat(count)),
          stat = "count",
          position = position_stack(vjust = 0.5),
          color = "#000000") +
        xlab("Feature") +
        ylab("Count") +
        scale_fill_viridis_d("Feature", end = 0.8) +
        theme
    },

    "roc" = {
      plot_precrec(object, curvetype = "ROC") +
        scale_color_viridis_d(begin = 0.5, guide = "none") +
        theme +
        theme(plot.title = element_blank(), legend.position = "none")

    },

    "prc" = {
      plot_precrec(object, curvetype = "PRC") +
        scale_color_viridis_d(begin = 0.5, guide = "none") +
        theme +
        theme(plot.title = element_blank(), legend.position = "none")
    },

    "threshold" = {
      measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = object$task_type))
      pred = object$clone(deep = TRUE)
      tab = data.table(prob = seq(from = 0, to = 1, by = 0.01))
      tab$score = map_dbl(tab$prob, function(p) pred$set_threshold(p)$score(measure))
      ggplot(tab,
        mapping = aes(
          x = .data[["prob"]],
          y = .data[["score"]])) +
        geom_line(color = viridis::viridis(1, begin = 0.5)) +
        xlab("Probability Threshold") +
        ylab(measure$id) +
        scale_color_viridis_d() +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.PredictionClassif = function(x, ...) {
  print(autoplot(x, ...))
}
