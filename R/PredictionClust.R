#' @title Plot for PredictionClust
#'
#' @description
#' Generates plots for [mlr3cluster::PredictionClust], depending on argument `type`:
#'
#' * `"scatter"` (default): scatterplot with correlation values
#' and colored cluster assignments.
#'
#' * `"sil"`: Silhouette plot with mean silhouette value as
#' a reference line.
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
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3cluster)
#' library(mlr3viz)
#'
#' task = tsk("usarrests")
#' learner = lrn("clust.kmeans", centers = 3)
#' object = learner$train(task)$predict(task)
#'
#' head(fortify(object))
#' autoplot(object, task)
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

       data = merge(object$data$tab, data)
       data$row_id = NULL
       data$partition = factor(data$partition)

       GGally::ggscatmat(data, color = "partition", ...)
     },

     "sil" = {
       require_namespaces(c("cluster", "ggfortify"))

       # prepare data
       d = dist(task$data(rows = row_ids))
       sil = cluster::silhouette(object$data$tab$partition, d)

       ggplot2::autoplot(sil, ...)
     },

     stopf("Unknown plot type '%s'", type)
  )
}
