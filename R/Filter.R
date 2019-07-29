#' @title Plot for Filter Scores
#'
#' @description
#' Generates plots for [mlr3filters::Filter].
#'
#' @param object ([mlr3::Filter]).
#' @param n (`integer(1)`)\cr
#'   Only include the first `n` features with highest importance.
#'   Defaults to all features.
#' @param ... (`any`):
#'   Additional argument, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3filters)
#' task = mlr_tasks$get("mtcars")
#' f = mlr_filters$get("correlation")
#' f$calculate(task)
#'
#' head(fortify(f))
#' autoplot(f, n = 5)
autoplot.Filter = function(object, n = Inf, ...) {
  data = head(fortify(object), n)
  ggplot(data = data, aes_string(x = "feature", y = "score")) + geom_bar(stat = "identity") +
    scale_x_discrete(limits = data$feature)
}

#' @export
fortify.Filter = function(model, data = NULL, ...) {
  as.data.table(model)
}
