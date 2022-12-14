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
#'   task = mlr_tasks$get("usarrests")
#'
#'   head(fortify(task))
#'   autoplot(task)
#' }
autoplot.TaskClust = function(object, type = "pairs", ...) { # nolint
  assert_string(type)

  switch(type,
    "pairs" = {
      require_namespaces("GGally")

      color = apply_theme(viridis::viridis(1, begin = 0.5), "grey")
      alpha = apply_theme(0.8, 1)

      GGally::ggpairs(object,
        upper = list(continuous = "cor",  combo = GGally::wrap("box_no_facet", fill = color, alpha = alpha), discrete = "count", na = "na"),
        lower = list(continuous = GGally::wrap("points", color = color), combo = GGally::wrap("facethist", fill = color, alpha = alpha), discrete = GGally::wrap("facetbar", fill = color, alpha = alpha), na = "na"),
        diag = list(continuous = GGally::wrap("densityDiag", color = color), discrete = GGally::wrap("barDiag", fill = color, alpha = alpha), na = "naDiag"),
        ...) +
        apply_theme(list(theme_mlr3()))
    },

    stopf("Unknown plot type '%s'", type)
  )
}
