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
#'
#' @param object ([mlr3::PredictionClassif]).
#' @template param_type
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
#' task = tsk("spam")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' object = learner$train(task)$predict(task)
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object, type = "roc")
#' autoplot(object, type = "prc")
autoplot.PredictionClassif = function(object, type = "stacked", ...) { # nolint
  # nolint
  assert_string(type)

  switch(type,
    "stacked" = {
      tab = melt(fortify(object)[, c("truth", "response")],
        measure.vars = c("truth", "response"))
      ggplot(tab, aes_string(fill = "value", x = "variable")) +
        geom_bar(...) +
        geom_label(stat = "count", aes_string(label = "..count.."))
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
plot.PredictionClassif = function(x, ...) {
  print(autoplot(x, ...))
}
