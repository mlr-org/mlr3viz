#' @title Plot for LearnerClassifRpart
#'
#' @description
#' Visualize trees for [mlr3::mlr_learners_classif.rpart] or
#' [mlr3::mlr_learners_regr.rpart] using the package \CRANpkg{ggparty}.
#'
#' Contrary to \CRANpkg{ggparty}, boxplots are shown in the terminal nodes for
#' regression trees.
#'
#' Note that learner-specific plots are experimental and subject to change.
#'
#' @param object ([mlr3::LearnerClassifRpart] | [mlr3::LearnerRegrRpart]).
#' @param ... (`any`):
#'   Additional arguments, passed down to [ggparty::autoplot.party()].
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("spam")
#' learner = lrn("classif.rpart")
#' learner$train(task)
#' autoplot(learner)
autoplot.LearnerClassifRpart = function(object, ...) { # nolint
  if (is.null(object$model)) {
    stopf("Learner '%s' must be trained first", object$id)
  }

  require_namespaces(c("partykit", "ggparty"))
  # FIXME: partykit::as.party does not work without the task :/
  autoplot(partykit::as.party(object$model), ...)
}
