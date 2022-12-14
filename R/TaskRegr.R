#' @title Plot for Regression Tasks
#'
#' @description
#' Generates plots for [mlr3::TaskRegr], depending on argument `type`:
#'   * `"target"`: Box plot of target variable (default).
#'   * `"pairs"`: Passes data and additional arguments down to
#'   [GGally::ggpairs()]. Color is set to target column.
#'
#' @param object ([mlr3::TaskRegr]).
#' @template param_type
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
#'   task = tsk("mtcars")
#'   task$select(c("am", "carb"))
#'
#'   head(fortify(task))
#'   autoplot(task)
#'   autoplot(task, type = "pairs")
#' }
autoplot.TaskRegr = function(object, type = "target", ...) { # nolint
  assert_string(type)

  switch(type,
    "target" = {
      ggplot(data = object,
      mapping = aes(
        y = .data[[object$target_names]])) +
      geom_boxplot(
        fill = apply_theme(viridis::viridis(1, begin = 0.5), "#ffffff"),
        alpha = apply_theme(0.8, 1),
        ...) +
        scale_x_discrete() +
        apply_theme(list(theme_mlr3())) +
        theme(
          axis.text.x.bottom = element_blank(),
          axis.title.x.bottom = element_blank())
    },

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

#' @export
plot.TaskRegr = function(x, ...) {
  print(autoplot(x, ...))
}
