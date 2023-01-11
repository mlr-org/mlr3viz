#' @title Plots for Regression Tasks
#'
#' @description
#' Visualizations for [mlr3::TaskRegr].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#'   * `"target"` (default): Box plot of the target variable.
#'   * `"pairs"`: Passes data to [GGally::ggpairs()].
#'      Color is set to target column.
#'
#' @param object ([mlr3::TaskRegr]).
#' @template param_type
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3viz)
#'
#'   task = tsk("mtcars")
#'   task$select(c("am", "carb"))
#'
#'   head(fortify(task))
#'   autoplot(task)
#'   autoplot(task, type = "pairs")
#' }
autoplot.TaskRegr = function(object, type = "target", theme = theme_minimal(), ...) { # nolint
  assert_string(type)

  switch(type,
    "target" = {
      ggplot(data = object,
      mapping = aes(
        y = .data[[object$target_names]])) +
      geom_boxplot(
        fill = viridis::viridis(1, begin = 0.5),
        alpha = 0.8,
        color = "#000000",
        linewidth = 0.5) +
        scale_x_discrete() +
        theme +
        theme(
          axis.text.x.bottom = element_blank(),
          axis.title.x.bottom = element_blank())
    },

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

#' @export
plot.TaskRegr = function(x, ...) {
  print(autoplot(x, ...))
}
