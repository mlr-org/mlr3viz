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
#' # classification
#' task = tsk("spam")
#' learner = lrn("classif.rpart", keep_model = TRUE)
#' learner$train(task)
#' autoplot(learner)
#'
#' # regression
#' task = tsk("boston_housing")
#' learner = lrn("regr.rpart", keep_model = TRUE)
#' learner$train(task)
#' autoplot(learner)
#' plot(learner)
autoplot.LearnerClassifRpart = function(object, ...) { # nolint
  if (is.null(object$model)) {
    stopf("Learner '%s' must be trained first", object$id)
  }
  if (is.null(object$model$model)) {
    stopf("Learner '%s' must be trained with `keep_model` set to `TRUE`", object$id)
  }

  require_namespaces(c("partykit", "ggparty"))
  autoplot(partykit::as.party(object$model), ...) +
    ggparty::geom_node_label(aes(label = paste0("n=", .data[["nodesize"]])), nudge_y = 0.03, ids = "terminal")
}

#' @importFrom graphics plot
#' @param x ([mlr3::LearnerClassifRpart] | [mlr3::LearnerRegrRpart]).
#' @rdname autoplot.LearnerClassifRpart
#' @export
plot.LearnerClassifRpart= function(x, ...) {
  print(autoplot(x, ...))
}
