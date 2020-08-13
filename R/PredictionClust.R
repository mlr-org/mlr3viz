#' @title Plot for PredictionClust
#'
#' @description
#' Generates plots for [mlr3cluster::PredictionClust], depending on argument `type`:
#'
#' * `"scatter"` (default): scatterplot with correlation values
#' and colored cluster assignments.
#'
#' @param object ([mlr3cluster::PredictionClust]).
#' @param task ([mlr3cluster::TaskClust]).
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
autoplot.PredictionClust = function(object, task, type = "scatter", ...) { # nolint
  assert_string(type)

  switch(type,
     "scatter" = {
       require_namespaces("GGally")

       # merge features and partitions
       data = data.table(row_id = 1:nrow(task$data()), task$data())
       data = merge(object$data$tab, data)
       data$row_id = NULL
       data$partition = factor(data$partition)

       GGally::ggscatmat(data, color = "partition", ...)
     },

     "sil" = {
       require_namespaces("cluster")

       # prepare data
       d = dist(task$data())
       widths = cluster::silhouette(object$data$tab$partition, d)

       data = as.data.frame(unclass(widths))
       data$cluster = as.factor(data$cluster)
       data = data[order(data$cluster), ]
       data$id = seq_len(nrow(data))

       min.y = if(min(data$sil_width) < 0) min(data$sil_width) else 0
       ggplot(data, aes(x = id, y = sil_width, fill = cluster)) +
         geom_bar(stat = "identity", color = "black") +
         ylim(min.y, 1) +
         labs(x = NULL, y = "Silhouette Width",
              title = sprintf("Average Silhouette Width: %s", round(mean(data$sil_width), 2))) +
         theme(axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               plot.title = element_text(hjust = 0.5),
               axis.title.y = element_text(hjust = 0.5)) +
         geom_hline(yintercept = round(mean(data$sil_width), 2), color = "red", linetype = "dashed") +
         coord_flip()
     },

     stopf("Unknown plot type '%s'", type)
  )
}
