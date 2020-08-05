#' @title Plot for LearnerClassifRpart
#'
#' @description
#' Visualize trees for [mlr3::mlr_learners_classif.rpart] or
#' [mlr3::mlr_learners_regr.rpart] using the package \CRANpkg{ggparty}.
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
  require_namespaces(c("partykit", "ggparty"))
  autoplot(partykit::as.party(object$model), ...)
}

#' @export
#' @rdname autoplot.LearnerClassifRpart
autoplot.LearnerRegrRpart = function(object, ...) { # nolint
  require_namespaces(c("partykit", "ggparty"))
  autoplot(partykit::as.party(object$model), ...)
}
