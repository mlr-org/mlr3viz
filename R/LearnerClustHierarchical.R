#' @title Plot for Hierarchical Clustering Learners
#'
#' @description
#' Generates plots for hierarchical clusterers, depending on argument `type`:
#'
#' * `"dend"` (default): dendrograms using \CRANpkg{ggdendro} package.
#'
#' * `"scree"`: scree plot that shows the number of possible clusters on x-axis and the height on the y-axis.
#'
#' Note that learner-specific plots are experimental and subject to change.
#'
#' @param object ([mlr3cluster::LearnerClustAgnes] | [mlr3cluster::LearnerClustDiana] | [mlr3cluster::LearnerClustHclust]).
#' @param task ([mlr3::Task])\cr
#'  Optionally, pass the task to add labels of observations to a hclust dendrogram.
#'  Labels are set via the row names of the task.
#' @template param_type
#' @param ... (`any`):
#'   Additional arguments, passed down to function `ggdendrogram()` in package \CRANpkg{ggdendro}.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3cluster)
#'   library(mlr3viz)
#'
#'   task = tsk("usarrests")
#'
#'   # agnes clustering
#'   learner = lrn("clust.agnes")
#'   learner$train(task)
#'   autoplot(learner)
#'
#'   # diana clustering
#'   learner = lrn("clust.diana")
#'   learner$train(task)
#'   autoplot(learner)
#'
#'   # hclust clustering
#'   learner = lrn("clust.hclust")
#'   learner$train(task)
#'   autoplot(learner, type = "scree")
#' }
autoplot.LearnerClustHierarchical = function(object, type = "dend", task = NULL, ...) { # nolint
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

      if (!is.null(task) && !is.null(task$row_names)) {
        object$model$labels = task$row_names$row_name[match(object$model$order, task$row_names$row_id)]
      }

      ggdendro::ggdendrogram(as.dendrogram(object$model), ...)
    },

    "scree" = {
      data = data.table(Height = object$model$height, Clusters = seq(length(object$model$height), 1))
      ggplot(data, aes(x = data$Clusters, y = data$Height)) +
        geom_line(color = apply_theme(viridis::viridis(1, begin = 0.5), "#000000")) +
        geom_point(
          size = 3,
          color = apply_theme(viridis::viridis(1, begin = 0.5), "#000000")) +
        xlab("Clusters") +
        ylab("Height") +
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
