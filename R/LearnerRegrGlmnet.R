#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerRegrGlmnet = function(object, theme = theme_minimal(), ...) { # nolint
  plot_ggfortify(object, ...) +
    scale_color_viridis_d("Feature") +
    theme
}

#' @export
plot.LearnerRegrGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
