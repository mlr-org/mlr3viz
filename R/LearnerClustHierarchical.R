#' @title Plot for Hierarchical Clustering Learners
#'
#' @description
#' Generates plots for hierarchical clusterers, depending on argument `type`:
#'
#' * `"dend"` (default): dendrograms using \CRANpkg{factoextra} package.
#'
#' * `"scree"`: scree plot that shows the number of possible clusters on x-axis and
#' the height on the y-axis.
#'
#' Note that learner-specific plots are experimental and subject to change.
#'
#' @param object ([mlr3cluster::LearnerClustAgnes] | [mlr3cluster::LearnerClustDiana] | [mlr3cluster::LearnerClustHclust]).
#' @template param_type
#' @param ... (`any`):
#'   Additional arguments, passed down to function [factoextra::fviz_dend()] in package \CRANpkg{factoextra}.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
#'
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
#'
#' # hclust clustering
#' learner = mlr_learners$get("clust.hclust")
#' learner$train(task)
#' autoplot(learner, type = "scree")
autoplot.LearnerClustHierarchical = function(object, type = "dend", ...) { # nolint
  assert_string(type)
  if (is.null(object$model)) {
    stopf("Learner '%s' must be trained first", object$id)
  }
  if (!("hierarchical" %in% object$properties)) {
    stopf("Learner '%s' must be hierarchical", object$id)
  }

  switch(type,
    "dend" = {
      require_namespaces("factoextra")

      p = factoextra::fviz_dend(object$model, horiz = FALSE, ggtheme = theme_gray(), main = NULL, ...)
      if (getOption("mlr3.theme", TRUE)) p$scales$scales = list()

      p +
        apply_theme(list(scale_color_viridis_d(end = 0.8), theme_mlr3())) +
        theme(legend.position = "none")
    },

    "scree" = {
      data = data.table(Height = object$model$height, Clusters = seq(length(object$model$height), 1))
      ggplot(data, aes(x = data$Clusters, y = data$Height)) + geom_point() + geom_line() +
        xlab("Clusters") + ylab("Height") +
        apply_theme(list(theme_mlr3()))
    }
  )
}

#' @export
autoplot.LearnerClustAgnes = autoplot.LearnerClustHierarchical

#' @export
autoplot.LearnerClustDiana = autoplot.LearnerClustHierarchical

#' @export
autoplot.LearnerClustHclust = autoplot.LearnerClustHierarchical
