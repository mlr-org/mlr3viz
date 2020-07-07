#' @title Plot for PredictionClust
#'
#' @description
#' Generates plots for [mlr3::PredictionClust], depending on argument `type`:
#'
#' * `"scatter"` (default): scatterplot with correlation values 
#' and colored cluster assignments.
#'
#' @param object ([mlr3::PredictionClust]).
#' @template param_type
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
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
       # merge features and partitions
       plot.data = data.table(row_id = 1:nrow(task$data()), task$data())
       plot.data = merge(pred$data$tab, plot.data)
       plot.data$row_id = NULL
       plot.data$partition = factor(plot.data$partition)
       
       ggscatmat(plot.data, color = "partition", ...)
     },
     
     stopf("Unknown plot type '%s'", type)
  )
}
