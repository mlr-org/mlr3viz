#' @title Plot for Classification Tasks
#'
#' @description
#' Generates plots for [mlr3::TaskClassif].
#'
#' @param object ([mlr3::TaskClassif]).
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task = mlr_tasks$get("iris")
#'
#' library(ggplot2)
#' autoplot(task)
#' autoplot(task, type = "pairs")
#' autoplot(task, type = "duo")
autoplot.TaskClassif = function(object, type = "target") {
  assert_choice(type, c("target", "pairs", "duo"))
  target = object$target_names
  if (type == "target") {
    ggplot(data = object, aes_string(x = target, fill = target)) + geom_bar(stat = "count")
  } else if (type == "pairs") {
    require_namespaces("GGally")
    GGally::ggpairs(object, aes_string(color = target))
  } else {
    features = object$feature_names
    GGally::ggduo(object, target, features, mapping = aes_string(color = target))
  }
}
