#' @title Plot for LearnerClassifGlmnet / LearnerRegrGlmnet / LearnerClassifCVGlmnet / LearnerRegrCVGlmnet
#'
#' @description
#' Visualizations for [mlr3learners::mlr_learners_classif.glmnet], [mlr3learners::mlr_learners_regr.glmnet],
#' [mlr3learners::mlr_learners_classif.cv_glmnet] and [mlr3learners::mlr_learners_regr.cv_glmnet]
#' using the package \CRANpkg{ggfortify}.
#'
#' Note that learner-specific plots are experimental and subject to change.
#'
#' @param object ([mlr3learners::LearnerClassifGlmnet] | [mlr3learners::LearnerRegrGlmnet] |
#'   [mlr3learners::LearnerRegrCVGlmnet] | [mlr3learners::LearnerRegrCVGlmnet]).
#' @param ... (`any`):
#'   Additional arguments, passed down to [ggparty::autoplot.party()].
#'
#' @return [ggplot2::ggplot()] object.
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
autoplot.LearnerClassifGlmnet = function(object, ...) { # nolint
  plot_ggfortify(object, ...)
}

#' @export
plot.LearnerClassifGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
