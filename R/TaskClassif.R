#' @title Plot for Classification Tasks
#'
#' @description
#' Generates plots for [mlr3::TaskClassif], depending on argument `type`:
#' * `"target"` (default): Bar plot of the target variable (default).
#' * `"duo"`: Passes data and additional arguments down to [GGally::ggduo()].
#'   `columnsX` is target, `columnsY` is features.
#' * `"pairs"`: Passes data and additional arguments down to
#' [GGally::ggpairs()]. Color is set to target column.
#'
#' @param object ([mlr3::TaskClassif]).
#' @template param_type
#' @param ... (`any`):
#'   Additional argument, possibly passed down to the underlying plot functions.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
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
autoplot.TaskClassif = function(object, type = "target", ...) { # nolint
  assert_string(type)

  target = object$target_names

  switch(type,
    "target" = {
      ggplot(object,
        mapping = aes(
          x = .data[[target]],
          fill = .data[[target]])) +
        geom_bar(
          stat = "count",
          color = "#000000", ...) +
        apply_theme(list(
          scale_fill_viridis_d(end = 0.8, alpha = 0.8, ),
          scale_color_viridis_d(end = 0.8),
          theme_mlr3()))
    },

    "duo" = {
      require_namespaces("GGally")
      GGally::ggduo(object,
        columnsX = target,
        columnsY = object$feature_names,
        mapping = aes(color = .data[[target]]),
        ...) +
        apply_theme(list(
          scale_fill_viridis_d(end = 0.8, alpha = 0.8),
          scale_color_viridis_d(end = 0.8),
          theme_mlr3())) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank()
        )
    },

    "pairs" = {
      require_namespaces("GGally")

      GGally::ggpairs(object,
        mapping = aes(color = .data[[target]]),
        upper = list(continuous = "cor",  combo = "box_no_facet", discrete = "count", na = "na"),
        lower = list(continuous = "points", combo = GGally::wrap("facethist", color = "#000000"), discrete = GGally::wrap("facetbar", color = "#000000"), na = "na"),
        diag = list(continuous = "densityDiag", discrete = GGally::wrap("barDiag", color = "#000000"), na = "naDiag"),
        ...) +
        apply_theme(list(
          scale_fill_viridis_d(end = 0.8, alpha = 0.8),
          scale_color_viridis_d(end = 0.8),
          theme_mlr3()))
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.TaskClassif = function(x, ...) {
  print(autoplot(x, ...))
}
