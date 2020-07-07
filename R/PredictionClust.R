#' @title Plot for PredictionClust
#'
#' @description
#' Generates plots for [mlr3cluster::PredictionClust], depending on argument `type`:
#' * `"scatter"` (default): scatterplot with correlation values and cluster assignments.
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
      plot_data = data.table(row_id = seq_len(nrow(task$data())), task$data())
      plot_data = merge(object$data$tab, plot_data)
      plot_data$row_id = NULL
      plot_data$partition = factor(plot_data$partition)

      GGally::ggscatmat(plot_data, color = "partition", ...)
    },

    stopf("Unknown plot type '%s'", type)
  )
}
