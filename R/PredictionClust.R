#' @title Plots for Cluster Predictions
#'
#' @description
#' Visualizations for [mlr3cluster::PredictionClust].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"scatter"` (default): scatterplot with correlation values and colored cluster assignments.
#' * `"sil"`: Silhouette plot with mean silhouette value as the reference line.
#'    Requires package \CRANpkg{ggfortify}.
#' * `"pca"`: Perform PCA on data and color code cluster assignments.
#'    Inspired by and uses [ggfortify::autoplot.kmeans].
#'
#' @param object ([mlr3cluster::PredictionClust]).
#' @param task ([mlr3cluster::TaskClust]).
#' @param row_ids (`integer()`)
#' Row ids to subset task data to ensure that only the data used to make predictions are shown in plots.
#' @template param_type
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
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3cluster)
#'   library(mlr3viz)
#'
#'   task = tsk("usarrests")
#'   learner = lrn("clust.kmeans", centers = 3)
#'   object = learner$train(task)$predict(task)
#'
#'   head(fortify(object))
#'   autoplot(object, task)
#' }
autoplot.PredictionClust = function(object, task, row_ids = NULL, type = "scatter", theme = theme_minimal(), ...) { # nolint
  assert_string(type)

  switch(type,
    "scatter" = {
      require_namespaces("GGally")

      # merge features and partitions
      if (is.null(row_ids)) {
        data = data.table(row_id = task$row_ids, task$data())
      } else {
        data = data.table(row_id = row_ids, task$data(rows = row_ids))
      }

      preds = data.table(row_id = object$data$row_ids, partition = object$data$partition)
      data = merge(preds, data)
      data$row_id = NULL
      data$partition = factor(data$partition)

      GGally::ggscatmat(data, color = "partition") +
        scale_color_viridis_d("Cluster", end = 0.8, alpha = 0.8) +
        theme +
        theme(axis.title.x.bottom = element_blank(), axis.title.y.left = element_blank())
    },

    "sil" = {
      require_namespaces(c("cluster", "ggfortify", "stats"))

      # prepare data
      d = stats::dist(task$data(rows = row_ids))
      sil = cluster::silhouette(object$data$partition, d)

      ggplot2::autoplot(sil, colour = "#000000") +
        scale_fill_viridis_d("Cluster", end = 0.8, alpha = 0.8) +
        theme
    },

    "pca" = {
      require_namespaces("ggfortify")
      d = data.frame(
        row_ids = object$data$row_id,
        cluster = as.factor(object$data$partition))

      if (is.null(row_ids)) {
        task_data = data.table(task$data(), row_ids = task$row_ids)
      } else {
        task_data = data.table(task$data(rows = row_ids), row_ids = row_ids)
      }

      plot_data = merge(task_data, d, by = "row_ids")
      ggplot2::autoplot(stats::prcomp(task_data[, -"row_ids"]),
        data = plot_data,
        colour = "cluster",
        size = 3) +
        scale_color_viridis_d("Cluster", end = 0.8, alpha = 0.8) +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}
