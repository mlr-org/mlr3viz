#' @title Plot for Survival Tasks
#'
#' @description
#' Generates plots for [mlr3proba::TaskSurv], depending on argument `type`:
#'   * `"target"`: Calls [GGally::ggsurv()] on a [survival::survfit()] object.
#'   * `"duo"`: Passes data and additional arguments down to [GGally::ggduo()].
#'     `columnsX` is target, `columnsY` is features.
#'   * `"pairs"`: Passes data and additional arguments down to
#'   [GGally::ggpairs()].
#'     Color is set to target column.
#'
#' @param object ([mlr3proba::TaskSurv]).
#' @param type (`character(1)`):\cr
#'   Type of the plot. Available choices:
#' @param ... (`any`):
#'   Additional argument, passed down to `$formula` of [mlr3proba::TaskSurv] or
#'   the underlying plot functions.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#' library(mlr3proba)
#'
#' task = tsk("lung")
#'
#' head(fortify(task))
#' autoplot(task)
#' plot(task)
#' autoplot(task, rhs = "sex")
#' autoplot(task, type = "duo")
autoplot.TaskSurv = function(object, type = "target", ...) { # nolint
  assert_string(type)
  require_namespaces(c("survival", "GGally"))

  switch(type,
    "target" = {
      if (...length() == 0L) {
        GGally::ggsurv(invoke(survival::survfit,
          formula = object$formula(1),
          data = object$data()))
      } else {
        GGally::ggsurv(invoke(survival::survfit,
          formula = object$formula(...),
          data = object$data()))
      }
    },

    "duo" = {
      GGally::ggduo(object,
        columnsX = object$target_names,
        columnsY = object$feature_names, ...)
    },

    "pairs" = {
      GGally::ggpairs(object, ...)
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @importFrom graphics plot
#' @param x ([mlr3proba::TaskSurv]).
#' @rdname autoplot.TaskSurv
#' @export
plot.TaskSurv = function(x, ...) {
  print(autoplot(x, ...))
}
