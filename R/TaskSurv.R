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
#'   Additional arguments.
#'   `rhs` is passed down to `$formula` of [mlr3proba::TaskSurv] for stratification
#'   for type `"target"`. Other arguments are passed to the respective underlying plot
#'   functions.
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
#'
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
#' autoplot(task, rhs = "sex")
#' autoplot(task, type = "duo")
autoplot.TaskSurv = function(object, type = "target", ...) { # nolint
  assert_string(type)
  require_namespaces(c("survival", "GGally"))

  switch(type,
    "target" = {
      ddd = list(...)

      sf = survival::survfit(
        object$formula(ddd$rhs %??% 1),
        data = object$data()
      )

      plot = GGally::ggsurv(sf, remove_named(ddd, "rhs"))
      plot + apply_theme(list(scale_color_viridis_d(end = 0.8), theme_mlr3()))
    },

    "duo" = {
      GGally::ggduo(object,
        columnsX = object$target_names,
        columnsY = object$feature_names, ...) +
        apply_theme(list(
          scale_color_viridis_d(end = 0.8),
          theme_mlr3()
        ))
    },

    "pairs" = {
      GGally::ggpairs(object, ...) +
        apply_theme(list(theme_mlr3(base_size = 10)))
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.TaskSurv = function(x, ...) {
  print(autoplot(x, ...))
}
