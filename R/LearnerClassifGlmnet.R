#' @title Plots for GLMNet Learners
#'
#' @description
#' Visualizations for [mlr3learners::LearnerClassifGlmnet].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"prediction"` (default): Decision boundary of the learner and the true class labels.
#' * `"ggfortify"`: Visualizes the model using the package \CRANpkg{ggfortify}.
#'
#' @param object ([mlr3learners::LearnerClassifGlmnet] | [mlr3learners::LearnerRegrGlmnet] | [mlr3learners::LearnerRegrCVGlmnet] | [mlr3learners::LearnerRegrCVGlmnet]).
#'
#' @template param_type
#' @template param_task
#' @template param_grid_points
#' @template param_expand_range
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#'
#' @references
#' `r format_bib("ggfortify")`
#'
#' @export
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3viz)
#' library(mlr3learners)
#'
#' # classification
#' task = tsk("sonar")
#' learner = lrn("classif.glmnet")
#' learner$train(task)
#' autoplot(learner, type = "ggfortify")
#'
#' # regression
#' task = tsk("mtcars")
#' learner = lrn("regr.glmnet")
#' learner$train(task)
#' autoplot(learner, type = "ggfortify")
#' }
autoplot.LearnerClassifGlmnet = function(object, type = "prediction", task = NULL, grid_points = 100L, expand_range = 0, theme = theme_minimal(), ...) { # nolint
  assert_choice(type, choices = c("prediction", "ggfortify"), null.ok = FALSE)
  assert_has_model(object)

  switch(type,
    "prediction" = {
      NextMethod()
    },

    "ggfortify" = {
      if ("twoclass" %nin% object$state$train_task$properties) {
        stopf("Plot of %s only works with binary classification tasks.", object$id)
      }

      plot_ggfortify(object) +
        scale_color_viridis_d("Feature") +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.LearnerClassifGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
