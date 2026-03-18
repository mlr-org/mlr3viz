#' @title Plots for Clustering Tasks
#'
#' @description
#' Visualizations for [mlr3cluster::TaskClust].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"pairs"` (default): Passes data [GGally::ggpairs()].
#'
#' @param object ([mlr3cluster::TaskClust]).
#' @template param_type
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
#' @examples
#' if (mlr3misc::require_namespaces("mlr3cluster", quietly = TRUE)) {
#' library(mlr3cluster)
#' task = tsk("usarrests")
#'
#' head(fortify(task))
#' autoplot(task)
#' }
#nolint next
autoplot.TaskClust = function(object, type = "pairs", theme = theme_minimal(), ...) {
  assert_choice(type, choices = c("pairs"), null.ok = FALSE)

  switch(
    type,
    "pairs" = {
      require_namespaces("GGally")

      color = viridis::viridis(1, begin = 0.5)
      alpha = 0.8

      GGally::ggpairs(
        object,
        upper = list(
          continuous = "cor",
          combo = GGally::wrap("box_no_facet", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5),
          discrete = "count",
          na = "na"
        ),
        lower = list(
          continuous = GGally::wrap("points", color = color, alpha = alpha),
          combo = GGally::wrap("facethist", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5),
          discrete = GGally::wrap("facetbar", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5),
          na = "na"
        ),
        diag = list(
          continuous = GGally::wrap("densityDiag", color = color),
          discrete = GGally::wrap("barDiag", fill = color, alpha = alpha, color = "#000000", linewidth = 0.5),
          na = "naDiag"
        )
      ) +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}
