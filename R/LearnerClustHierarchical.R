#' @title Plots for Hierarchical Clustering Learners
#'
#' @description
#' Visualizations for hierarchical clusters.
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"dend"` (default): Dendrograms using \CRANpkg{ggdendro} package.
#' * `"scree"`: Scree plot that shows the number of possible clusters on the x-axis and the height on the y-axis.
#'
#' @param object ([mlr3cluster::LearnerClustAgnes] | [mlr3cluster::LearnerClustDiana] | [mlr3cluster::LearnerClustHclust]).
#' @param task ([mlr3::Task])\cr
#'  Optionally, pass the task to add labels of observations to a `hclust` dendrogram.
#'  Labels are set via the row names of the task.
#' @template param_type
#' @template param_theme
#' @param theme_dendro (`logical(1)`)\cr
#'  If `TRUE` (default), the special dendrogram theme from \CRANpkg{ggdendro} package is used in plot `"dend"`.
#'  Set to `FALSE` to use the theme passed in `theme`.
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
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
autoplot.LearnerClustHierarchical = function(object, type = "dend", task = NULL, theme = theme_minimal(), theme_dendro = TRUE, ...) { # nolint
  assert_choice(type, choices = c("dend", "scree"), null.ok = FALSE)

  if (is.null(object$model)) {
    stopf("Learner '%s' must be trained first", object$id)
  }
  if (!("hierarchical" %in% object$properties)) {
    stopf("Learner '%s' must be hierarchical", object$id)
  }

  switch(type,
    "dend" = {
      require_namespaces("ggdendro")

      if (!is.null(task) && !is.null(task$row_names)) {
        object$model$labels = task$row_names$row_name[match(object$model$order, task$row_names$row_id)]
      }

      ggdendro::ggdendrogram(as.dendrogram(object$model), theme_dendro = theme_dendro, ...) +
        if (!theme_dendro) theme else geom_blank()
    },

    "scree" = {
      data = data.table(Height = object$model$height, Clusters = seq(length(object$model$height), 1))
      ggplot(data,
        mapping = aes(x = data$Clusters, y = data$Height)) +
        geom_line(color = viridis::viridis(1, begin = 0.5)) +
        geom_point(
          size = 3,
          color = viridis::viridis(1, begin = 0.5),
          alpha = 0.8) +
        xlab("Clusters") +
        ylab("Height") +
        theme
    }
  )
}

#' @export
autoplot.LearnerClustAgnes = autoplot.LearnerClustHierarchical

#' @export
autoplot.LearnerClustDiana = autoplot.LearnerClustHierarchical

#' @export
autoplot.LearnerClustHclust = autoplot.LearnerClustHierarchical
