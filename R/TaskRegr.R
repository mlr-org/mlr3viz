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
#' @param ... (`any`):
#'   Additional argument, passed down to the underlying `geom` or plot
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
#'
#' task = tsk("mtcars")
#' task$select(c("am", "carb"))
#'
#' head(fortify(task))
#' autoplot(task)
#' autoplot(task, type = "pairs")
autoplot.TaskRegr = function(object, type = "target", ...) { # nolint
  assert_string(type)

  switch(type,
    "target" = {
      target = object$target_names
      ggplot(data = object, aes_string(
        x = as.factor(target), y = target)) +
        geom_boxplot(...) +
        apply_theme(list(theme_mlr3())) +
        theme(axis.text.x.bottom = element_blank(), axis.title.x.bottom = element_blank())
    },

    "pairs" = {
      require_namespaces("GGally")
      GGally::ggpairs(object, ...) +
        apply_theme(list(theme_mlr3()))
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.TaskRegr = function(x, ...) {
  print(autoplot(x, ...))
}
