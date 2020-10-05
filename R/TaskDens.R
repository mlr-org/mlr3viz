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
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' task = tsk("precip")
#'
#' head(fortify(task))
#' autoplot(task, bins = 15)
#' plot(task, bins = 15)
#' autoplot(task, type = "freq", bins = 15)
#' autoplot(task, type = "overlay", bins = 15)
#' autoplot(task, type = "freqpoly", bins = 15)
autoplot.TaskDens = function(object, type = "dens", ...) {
  assert_choice(type, c("dens", "freq", "overlay", "freqpoly"))

  p = ggplot(data = object, aes_string(x = object$target_names), ...)
  # hacky density fix
  ..density.. <- NULL

  if (type == "dens") {
    p + geom_histogram(aes(y = ..density..), fill = "white", color = "black", ...)
  } else if (type == "freq") {
    p + geom_histogram(fill = "white", color = "black", ...)
  } else if (type == "overlay") {
    p +
      geom_histogram(aes(y = ..density..), colour = "black", fill = "white", ...) +
      geom_density(alpha = 0.2, fill = "#FF6666")
  } else {
    p + geom_freqpoly(...)
  }
}

#' @importFrom graphics plot
#' @param x ([mlr3proba::TaskDens]).
#' @rdname autoplot.TaskDens
#' @export
plot.TaskDens = function(x, ...) {
  print(autoplot(x, ...))
}
