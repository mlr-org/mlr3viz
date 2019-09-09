#' @title Receiver Operatoring Characteristic Plots
#'
#' @description
#' **This function is not finished yet. Do not use.**
#'
#' Takes a [mlr3::PredictionClassif] for a binary classification problem and
#' plots a ROC curve.
#' Internally calls the function [precrec::evalmod()] from package \CRANpkg{precrec}.
#'
#' @param prediction :: `[mlr3::PredictionClassif].
#'
#' @return Object of type [ggplot2::ggplot()].
#' @export
#' @keywords internal
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("spam")
#' lrn = mlr_learners$get("classif.rpart")
#' lrn$predict_type = "prob"
#' prediction = lrn$train(task)$predict(task)
#' p = roc(prediction)
roc = function(prediction) {

  # TODO:
  # 1) This might not be the best interface, we have autoplot everywhere else.
  # 2) We need to expose more parameters to allow putting arbitrary measures on the axes.
  # 3) precrec can average multiple ROC curves and give you confidence bounds.
  #    If roc() is called on a ResampleResult (instead of a Prediction), we should use this.
  assert_class(prediction, "PredictionClassif")
  truth = prediction$truth
  if (nlevels(truth) != 2L) {
    stopf("Need a binary classification problem to plot a ROC curve")
  }
  if ("prob" %nin% prediction$predict_types) {
    stopf("Need predicted probabilities to plot a ROC curve")

  }

  require_namespaces("precrec")
  positive = levels(truth)[1L]
  evaluated = precrec::evalmod(
    scores = prediction$prob[, positive, drop = TRUE],
    label = truth,
    posclass = positive
  )
  autoplot(evaluated, curvetype = "ROC")
}
