#' @title Plots for Classification Tasks
#'
#' @description
#' Visualizations for [mlr3::TaskClassif].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"target"` (default): Bar plot of the target variable (default).
#' * `"duo"`: Passes data to [GGally::ggduo()].
#'   `columnsX` is the target and `columnsY` are the features.
#' * `"pairs"`: Passes data to [GGally::ggpairs()].
#'   Color is set to target column.
#'
#' @param object ([mlr3::TaskClassif]).
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
#'   task = tsk("iris")
#'
#'   head(fortify(task))
#'   autoplot(task)
#'   autoplot(task$clone()$select(c("Sepal.Length", "Sepal.Width")),
#'     type = "pairs")
#'   autoplot(task, type = "duo")
#' }
autoplot.TaskClassif = function(object, type = "target", theme = theme_minimal(), ...) { # nolint
  assert_choice(type, choices = c("target", "duo", "pairs"), null.ok = FALSE)

  target = object$target_names

  switch(type,
    "target" = {
      ggplot(object,
        mapping = aes(
          x = .data[[target]],
          fill = .data[[target]])) +
        geom_bar(
          stat = "count",
          color = "#000000",
          linewidth = 0.5) +
        scale_fill_viridis_d(end = 0.8, alpha = 0.8, ) +
        scale_color_viridis_d(end = 0.8) +
        theme
    },

    "duo" = {
      # Line width!!!
      require_namespaces("GGally")
      GGally::ggduo(object,
        columnsX = target,
        columnsY = object$feature_names,
        mapping = aes(color = .data[[target]])) +
        scale_fill_viridis_d(end = 0.8, alpha = 0.8) +
        scale_color_viridis_d(end = 0.8) +
        theme +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank()
        )
    },

    "pairs" = {
      require_namespaces("GGally")

      GGally::ggpairs(object,
        mapping = aes(color = .data[[target]]),
        upper = list(continuous = "cor",  combo = GGally::wrap("box_no_facet", color = "#000000", linewidth = 0.5), discrete = "count", na = "na"),
        lower = list(continuous = GGally::wrap("points", size = 3, alpha = 0.8) , combo = GGally::wrap("facethist", color = "#000000", linewidth = 0.5), discrete = GGally::wrap("facetbar", color = "#000000", linewidth = 0.5), na = "na"),
        diag = list(continuous = GGally::wrap("densityDiag", color = "#000000", linewidth = 0.5), discrete = GGally::wrap("barDiag",  color = "#000000", linewidth = 0.5), na = "naDiag")) +
        scale_fill_viridis_d(end = 0.8, alpha = 0.8) +
        scale_color_viridis_d(end = 0.8, alpha = 0.8) +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.TaskClassif = function(x, ...) {
  print(autoplot(x, ...))
}
