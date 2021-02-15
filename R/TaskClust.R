#' @title Plot for Clustering Tasks
#'
#' @description
#' Generates plots for [mlr3cluster::TaskClust], depending on argument `type`:
#' * `"pairs"`: Passes data and additional arguments down to
#'   [GGally::ggpairs()] (default).
#'
#' @param object ([mlr3cluster::TaskClust]).
#' @template param_type
#' @param ... (`any`):
#'   Additional argument, passed down to the underlying `geom` or plot
#'   functions.
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
#' head(fortify(task))
#' autoplot(task)
autoplot.TaskClust = function(object, type = "pairs", ...) { # nolint
  assert_string(type)

  switch(type,
    "pairs" = {
      require_namespaces("GGally")
      GGally::ggpairs(object, ...)
    },

    stopf("Unknown plot type '%s'", type)
  )
}
