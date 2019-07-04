#' @title Plot for Classification Tasks
#'
#' @description
#' Generates plots for [mlr3::TaskClassif].
#'
#' @param object ([mlr3::TaskClassif]).
#' @param type (`character(1)`):
#'   Type of the plot. Available choices:
#'   * `"target"`: bar plot of target variable (default).
#'   * `"duo"`: Passes data and additional arguments down to [GGally::ggduo].
#'     `columnsX` is target, `columnsY` is features.
#'   * `"pairs"`: Passes data and additional arguments down to [GGally::ggpairs].
#'     Color is set to target column.
#' @param ... (`any`):
#'   Additional argument, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("iris")
#'
#' head(fortify(task))
#' autoplot(task)
#' autoplot(task$clone()$select(c("Sepal.Length", "Sepal.Width")), type = "pairs")
#' autoplot(task, type = "duo")
autoplot.TaskClassif = function(object, type = "target", ...) {
  assert_choice(type, c("target", "duo", "pairs"))
  target = object$target_names

  if (type == "target") {
    ggplot(object, aes_string(x = target, fill = target)) + geom_bar(stat = "count")
  } else if (type == "pairs") {
    require_namespaces("GGally")
    GGally::ggpairs(object, aes_string(color = target), ...)
  } else {
    features = object$feature_names
    GGally::ggduo(object, columnsX = target, columnsY = features, mapping = aes_string(color = target), ...)
  }
}
