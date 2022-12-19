#' @title Plot for Clustering Tasks
#'
#' @description
#' Generates plots for [mlr3cluster::TaskClust], depending on argument `type`:
#' * `"pairs"`: Passes data and additional arguments down to
#'   [GGally::ggpairs()] (default).
#'
#' @param object ([mlr3cluster::TaskClust]).
#' @template param_type
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3cluster)
#'   library(mlr3viz)
#'
#'   task = mlr_tasks$get("usarrests")
#'
#'   head(fortify(task))
#'   autoplot(task)
#' }
autoplot.TaskClust = function(object, type = "pairs", theme = theme_minimal(), ...) { # nolint
  assert_string(type)

  switch(type,
    "pairs" = {
      require_namespaces("GGally")

      color = viridis::viridis(1, begin = 0.5)
      alpha = 0.8

      GGally::ggpairs(object,
        upper = list(continuous = "cor",  combo = GGally::wrap("box_no_facet", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5), discrete = "count", na = "na"),
        lower = list(continuous = GGally::wrap("points", color = color, alpha = alpha), combo = GGally::wrap("facethist", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5), discrete = GGally::wrap("facetbar", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5), na = "na"),
        diag = list(continuous = GGally::wrap("densityDiag", color = color), discrete = GGally::wrap("barDiag", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5), na = "naDiag")) +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}
