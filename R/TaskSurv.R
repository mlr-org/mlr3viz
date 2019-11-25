#' @title Plot for Survival Tasks
#'
#' @description
#' Generates plots for [mlr3proba::TaskSurv].
#'
#' @param object ([mlr3proba::TaskSurv]).
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
#' library(mlr3proba)
#' task = mlr_tasks$get("lung")
#'
#' head(fortify(task))
#' autoplot(task)
#' autoplot(task, rhs = "sex")
#' autoplot(task, type = "duo")
autoplot.TaskSurv = function(object, type = "target", ...) {
  assert_choice(type, c("target", "pairs", "duo"))
  target = object$target_names

  if (type == "target") {
    require_namespaces(c("survival", "GGally"))
    if (...length() == 0L) {
      GGally::ggsurv(invoke(survival::survfit, formula = object$formula(1), data = object$data()))
    } else {
      GGally::ggsurv(invoke(survival::survfit, formula = object$formula(...), data = object$data()))
    }
  } else
    if (type == "pairs") {
    require_namespaces("GGally")
    GGally::ggpairs(object, ...)
  } else {
    features = object$feature_names
    GGally::ggduo(object, columnsX = target, columnsY = features, ...)
  }
}
