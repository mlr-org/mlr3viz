#' @title Plot for Classification Tasks
#'
#' @description
#' Generates plots for [mlr3::TaskClassif], depending on argument `type`:
#' * `"target"` (default): Bar plot of the target variable (default).
#' * `"duo"`: Passes data and additional arguments down to [GGally::ggduo()].
#'   `columnsX` is target, `columnsY` is features.
#' * `"pairs"`: Passes data and additional arguments down to [GGally::ggpairs()].
#'   Color is set to target column.
#'
#' @param object ([mlr3::TaskClassif]).
#' @template param_type
#' @param ... (`any`):
#'   Additional argument, possibly passed down to the underlying plot functions.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = mlr_tasks$get("iris")
#'
#' head(fortify(task))
#' autoplot(task)
#' autoplot(task$clone()$select(c("Sepal.Length", "Sepal.Width")), type = "pairs")
#' autoplot(task, type = "duo")
autoplot.TaskClassif = function(object, type = "target", ...) {
  assert_string(type)

  target = object$target_names

  switch(type,
    "target" = {
      ggplot(object, aes_string(x = target, fill = target)) + geom_bar(stat = "count")
    },

    "duo" = {
      require_namespaces("GGally")
      GGally::ggduo(object, columnsX = target, columnsY = object$feature_names, mapping = aes_string(color = target), ...)
    },

    "pairs" = {
      require_namespaces("GGally")
      GGally::ggpairs(object, aes_string(color = target), ...)
    },

    stopf("Unknown plot type '%s'", type)
  )
}
