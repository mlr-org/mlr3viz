#' @title Plots for Filter Scores
#'
#' @description
#' Visualizations for [mlr3filters::Filter].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"barplot"` (default): Bar plot of filter scores.
#'
#' @param object ([mlr3filters::Filter]).
#' @template param_type
#' @param n (`integer(1)`)\cr
#'  Only include the first `n` features with the highest importance.
#'  Defaults to all features.
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#' @export
#' @examples
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3viz)
#'   library(mlr3filters)
#'
#'   task = tsk("mtcars")
#'   f = flt("correlation")
#'   f$calculate(task)
#'
#'   head(fortify(f))
#'   autoplot(f, n = 5)
#' }
autoplot.Filter = function(object, type = "boxplot", n = Inf, theme = theme_minimal(), ...) { # nolint
  assert_choice(type, choices = c("boxplot"), null.ok = FALSE)

  data = head(fortify(object), n)

  switch(type,
    "boxplot" = {
      ggplot(data,
        mapping = aes(
          x = .data[["feature"]],
          y = .data[["score"]])) +
        geom_bar(
          stat = "identity",
          fill = viridis::viridis(1, begin = 0.5),
          alpha = 0.8,
          color = "#000000") +
        scale_x_discrete(limits = data$feature) +
        xlab("Feature") +
        ylab("Score") +
        theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.Filter = function(x, ...) {
  print(autoplot(x, ...))
}

#' @export
fortify.Filter = function(model, data = NULL, ...) { # nolint
  as.data.table(model)
}
