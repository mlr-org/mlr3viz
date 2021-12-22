#' @title Plot for Density Tasks
#'
#' @description
#' Generates plots for [mlr3proba::TaskDens].
#'
#' @param object ([mlr3proba::TaskDens]).
#' @param type (`character(1)`):
#'   Type of the plot. Available choices:
#'   * `"dens"`: histogram density estimator (default) with [ggplot2::geom_histogram()].
#'   * `"freq"`: histogram frequency plot with [ggplot2::geom_histogram()].
#'   * `"overlay"`: histogram with overlaid density plot with [ggplot2::geom_histogram()] and
#'   [ggplot2::geom_density()].
#'   * `"freqpoly"`: frequency polygon plot with `ggplot2::geom_freqpoly`.
#' @param ... (`any`):
#'   Additional arguments, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' task = tsk("precip")
#'
#' head(fortify(task))
#' autoplot(task, bins = 15)
#' autoplot(task, type = "freq", bins = 15)
#' autoplot(task, type = "overlay", bins = 15)
#' autoplot(task, type = "freqpoly", bins = 15)
autoplot.TaskDens = function(object, type = "dens", ...) { # nolint
  assert_choice(type, c("dens", "freq", "overlay", "freqpoly"))

  p = ggplot(data = object, aes_string(x = object$feature_names), ...)
  # hacky density fix
  ..density.. = NULL

  if (type == "dens") {
    p +
      geom_histogram(aes(y = ..density..), fill = "white", color = "black", ...) +
      ylab("Density") +
      apply_theme(list(theme_mlr3()))
  } else if (type == "freq") {
    p + geom_histogram(fill = "white", color = "black", ...) +
      ylab("Count") +
      apply_theme(list(theme_mlr3()))
  } else if (type == "overlay") {
    p +
      geom_histogram(aes(y = ..density..), colour = "black", fill = "white", ...) +
      geom_density(alpha = 0.2, fill = apply_theme(viridis::viridis(1), "#3366FF")) +
      ylab("Density") +
      apply_theme(list(theme_mlr3()))
  } else {
    p +
      geom_freqpoly(...) +
      apply_theme(list(theme_mlr3()))
  }
}

#' @export
plot.TaskDens = function(x, ...) {
  print(autoplot(x, ...))
}
