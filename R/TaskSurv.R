#' @title Plot for Survival Tasks
#'
#' @description
#' Generates plots for [mlr3survival::TaskSurv].
#'
#' @param object ([mlr3survival::TaskSurv]).
#' @param type (`character(1)`):\cr
#'   Type of the plot. Available choices:
#'   * `"target"`: bar plot of target variable (default).
#'   * `"duo"`: Passes data and additional arguments down to [GGally::ggduo].
#'     `columnsX` is target, `columnsY` is features.
#'   * `"pairs"`: Passes data and additional arguments down to [GGally::ggpairs].
#'     Color is set to target column.
#' @param ... (any):\cr
#'   Additional argument, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3survival)
#' task = mlr_tasks$get("lung")
#'
#' library(ggplot2)
#' autoplot(task)
#' autoplot(task, strata = "sex")
#' autoplot(task, type = "duo")
autoplot.TaskSurv = function(object, type = "target", ...) {
  assert_choice(type, c("target", "pairs", "duo"))
  target = object$target_names

  if (type == "target") {
    require_namespaces(c("survival", "GGally"))
    GGally::ggsurv(object$survfit(...))
  } else if (type == "pairs") {
    require_namespaces("GGally")
    GGally::ggpairs(object, ...)
  } else {
    features = object$feature_names
    GGally::ggduo(object, columnsX = target, columnsY = features, ...)
  }
}

