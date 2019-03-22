#' @title Plot for Regression Tasks
#'
#' @description
#' Generates plots for [mlr3::TaskRegr].
#'
#' @param object ([mlr3::TaskRegr]).
#'
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
autoplot.TaskRegr = function(object, type = "target") {
  assert_choice(type, c("target", "pairs"))
  target = object$target_names
  if (type == "target") {
    ggplot(data = object, aes_string(x = as.factor(target), y = target, fill = target)) + geom_boxplot() + xlab("")
  } else {
    require_namespaces("GGally")
    GGally::ggpairs(object)
  }
}
