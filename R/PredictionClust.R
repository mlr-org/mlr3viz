#' @title Plot for PredictionClust
#'
#' @description
#' Generates plots for [mlr3cluster::PredictionClust], depending on argument `type`:
#'
#' * `"scatter"` (default): scatterplot with correlation values
#' and colored cluster assignments.
#'
#' * `"sil"`: Silhouette plot with mean silhouette value as
#'   a reference line. Requires package \CRANpkg{ggfortify}.
#'
#' * `"pca"`: Perform PCA on data and color code cluster
#'   assignments. Inspired by and uses [ggfortify::autoplot.kmeans].
#'
#' @param object ([mlr3cluster::PredictionClust]).
#' @param task ([mlr3cluster::TaskClust]).
#' @param row_ids row ids to subset task data to ensure that
#' only the data used to make predictions are shown in plots.
#' @template param_type
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
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
autoplot.PredictionClust = function(object, task, row_ids = NULL, type = "scatter", ...) { # nolint
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

      GGally::ggscatmat(data, color = "partition", ...) +
        apply_theme(list(
          scale_color_viridis_d("Cluster", end = 0.8),
          theme_mlr3() +
            theme(axis.title.x.bottom = element_blank(), axis.title.y.left = element_blank())
        ))
    },

    "sil" = {
      require_namespaces(c("cluster", "ggfortify", "stats"))

      # prepare data
      d = stats::dist(task$data(rows = row_ids))
      sil = cluster::silhouette(object$data$partition, d)

      ggplot2::autoplot(sil, ...) +
        apply_theme(list(
          scale_fill_viridis_d("Cluster", end = 0.8),
          theme_mlr3()
        ))
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
        colour = "cluster", ...) +
        apply_theme(list(
          scale_color_viridis_d("Cluster", end = 0.8),
          theme_mlr3()
        ))
    },

    stopf("Unknown plot type '%s'", type)
  )
}
