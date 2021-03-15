#' @title Plot for Hierarchical Clustering Learners
#'
#' @description
#' Visualize dendrograms for hierarchical clusterers
#' using the package \CRANpkg{factoextra}.
#'
#' Note that learner-specific plots are experimental and subject to change.
#'
#' @param object ([mlr3cluster::LearnerClustAgnes] | [mlr3cluster::LearnerClustDiana]).
#' @param ... (`any`):
#'   Additional arguments, passed down to function [factoextra::fviz_dend()] in package \CRANpkg{factoextra}.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3cluster)
#' library(mlr3viz)
#'
#' task = mlr_tasks$get("usarrests")
#'
#' # agnes clustering
#' learner = mlr_learners$get("clust.agnes")
#' learner$train(task)
#' autoplot(learner)
#'
#' # diana clustering
#' learner = mlr_learners$get("clust.diana")
#' learner$train(task)
#' autoplot(learner,
#'   k = learner$param_set$values$k, rect_fill = TRUE,
#'   rect = TRUE, rect_border = "red")
autoplot.LearnerClustHierarchical = function(object, ...) { # nolint
  if (is.null(object$model)) {
    stopf("Learner '%s' must be trained first", object$id)
  }
  if (!("hierarchical" %in% object$properties)) {
    stopf("Learner '%s' must be hierarchical", object$id)
  }
  require_namespaces("factoextra")

  factoextra::fviz_dend(object$model, horiz = FALSE, ggtheme = theme_gray(), main = NULL, ...)
}

#' @export
autoplot.LearnerClustAgnes = autoplot.LearnerClustHierarchical

#' @export
autoplot.LearnerClustDiana = autoplot.LearnerClustHierarchical

#' @export
autoplot.LearnerClustHclust = autoplot.LearnerClustHierarchical
