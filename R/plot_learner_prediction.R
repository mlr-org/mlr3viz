#' @title Plot for Learner Predictions
#'
#' @description
#' Generates plots for [mlr3::Learner], and [mlr3::Task].
#'
#' * For classification we support tasks with two features and learners with `predict_type="response"` and `"prob"`.
#' * For regression we support tasks with one or two features.
#'   For tasks with one feature we can print confidence bounds if the predict type of the learner was set to `"se"`.
#'   For tasks with two features the predict type will be ignored.
#'
#' @param learner ([mlr3::Learner])
#' @param task ([mlr3::Task])
#' @param grid_points (`integer(1)`)\cr
#'   Resolution of the grid.
#'   For factors, ordered and logicals this value is ignored.
#' @param expand_range (`numeric(1)`)\cr
#'   Expand the prediction range for numerical features.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("pima")$select(c("age", "glucose"))
#' learner = lrn("classif.rpart", predict_type = "prob")
#' p = plot_learner_prediction(learner, task)
plot_learner_prediction = function(learner, task, grid_points = 100L, expand_range = 0) {
  object = resample(task, learner$clone(), rsmp("holdout"), store_models = TRUE)
  autoplot(object, type = "prediction")
}


