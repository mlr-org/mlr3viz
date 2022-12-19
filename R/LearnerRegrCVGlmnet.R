#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerRegrCVGlmnet = function(object, theme = theme_minimal(), ...) { # nolint
  plot_ggfortify(object) +
    scale_color_viridis_d("Feature", end = 0.8) +
    theme
}

#' @export
plot.LearnerRegrCVGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
