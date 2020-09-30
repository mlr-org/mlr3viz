#' @title Plot for Filter Scores
#'
#' @description
#' Generates plots for [mlr3filters::Filter], depending on argument `type`:
#' * `"barplot"` (default): Bar plot of filter scores.
#'
#' @param object ([mlr3filters::Filter]).
#' @template param_type
#' @param n (`integer(1)`)\cr
#'   Only include the first `n` features with highest importance.
#'   Defaults to all features.
#' @param ... (`any`):
#'   Additional argument, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#' library(mlr3filters)
#'
#' task = tsk("mtcars")
#' f = flt("correlation")
#' f$calculate(task)
#'
#' head(fortify(f))
#' autoplot(f, n = 5)
#' plot(f, n = 5)
autoplot.Filter = function(object, type = "boxplot", n = Inf, ...) { # nolint
  assert_string(type)

  data = head(fortify(object), n)

  switch(type,
    "boxplot" = {
      ggplot(data = data, aes_string(x = "feature", y = "score")) +
        geom_bar(stat = "identity", ...) +
        scale_x_discrete(limits = data$feature)
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @importFrom graphics plot
#' @param x ([mlr3filters::Filter]).
#' @rdname autoplot.Filter
#' @export
plot.Filter = function(x, ...) {
  print(autoplot(x, ...))
}

#' @export
fortify.Filter = function(model, data = NULL, ...) { # nolint
  as.data.table(model)
}
