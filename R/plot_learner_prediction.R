#' @title Plots for Learner Predictions
#'
#' @description
#' Visualizations for the [mlr3::Prediction] of a single [mlr3::Learner] on a single [mlr3::Task].
#'
#' * For classification we support tasks with exactly two features and learners with `predict_type` set to `"response"` or `"prob"`.
#' * For regression we support tasks with one or two features.
#'   For tasks with one feature we print confidence bounds if the predict type of the learner was set to `"se"`.
#'   For tasks with two features the predict type will be ignored.
#'
#' Note that this function is a wrapper around [autoplot.ResampleResult()] for a temporary [mlr3::ResampleResult] using [mlr3::mlr_resamplings_holdout] with ratio 1 (all observations in the training set).
#'
#' @param learner ([mlr3::Learner]).
#' @param task ([mlr3::Task]).
#' @param grid_points (`integer(1)`)\cr
#'  Resolution of the grid.
#'  For factors, ordered and logicals this value is ignored.
#' @param expand_range (`numeric(1)`)\cr
#'  Expand the prediction range for numerical features.
#'
#' @return [ggplot2::ggplot()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3viz)
#'
#'   task = mlr3::tsk("pima")$select(c("age", "glucose"))
#'   learner = lrn("classif.rpart", predict_type = "prob")
#'   p = plot_learner_prediction(learner, task)
#'   print(p)
#' }
#' }
plot_learner_prediction = function(learner, task, grid_points = 100L, expand_range = 0) {
  object = mlr3::resample(task, learner$clone(), mlr3::rsmp("holdout", ratio = 1), store_models = TRUE)
  autoplot(object, type = "prediction", predict_sets = "train")
}
