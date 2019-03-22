#' @title Plot for Regression Tasks
#'
#' @description
#' Generates plots for [mlr3::TaskRegr].
#'
#' @param object ([mlr3::TaskRegr]).
#' @param type (`character(1)`)\cr
#'   Type of the plot:
#'   * `"target"`: bar plot of target variable (default).
#'   * `"pairs"`: Passes data and additional arguments down to [GGally::ggpairs].
#'     Color is set to target column.
#' @param ... (`any`)\cr
#'   Additional argument, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("mtcars")
#' task$select(c("mpg", "am", "carb"))
#'
#' library(ggplot2)
#' autoplot(task)
#' autoplot(task, type = "pairs")
autoplot.TaskRegr = function(object, type = "target", ...) {
  assert_choice(type, c("target", "pairs"))
  target = object$target_names

  if (type == "target") {
    ggplot(data = object, aes_string(x = as.factor(target), y = target, fill = target)) + geom_boxplot() + xlab("")
  } else {
    require_namespaces("GGally")
    GGally::ggpairs(object, ...)
  }
}
