#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerRegrGlmnet = function(object, ...) { # nolint
  plot_ggfortify(object, ...) +
    apply_theme(list(
      scale_color_viridis_d("Feature", end = 0.8),
      theme_mlr3()
    ))
}

#' @export
plot.LearnerRegrGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
