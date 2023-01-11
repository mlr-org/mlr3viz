#' @title Plots for GLMNet Learners
#'
#' @description
#' Visualizations for GLMNet learners using the package \CRANpkg{ggfortify}.
#'
#' @param object ([mlr3learners::LearnerClassifGlmnet] | [mlr3learners::LearnerRegrGlmnet] | [mlr3learners::LearnerRegrCVGlmnet] | [mlr3learners::LearnerRegrCVGlmnet]).
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
#' autoplot(learner)
#'
#' # regression
#' task = tsk("mtcars")
#' learner = lrn("regr.glmnet")
#' learner$train(task)
#' autoplot(learner)
#' }
autoplot.LearnerClassifGlmnet = function(object, theme = theme_minimal(), ...) { # nolint
  if ("twoclass" %nin% object$state$train_task$properties) {
    stopf("Plot of %s only works with binary classification tasks.", object$id)
  }

  plot_ggfortify(object) +
      scale_color_viridis_d("Feature") +
      theme
}

#' @export
plot.LearnerClassifGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
