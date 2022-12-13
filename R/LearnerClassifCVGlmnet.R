#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerClassifCVGlmnet = function(object, ...) { # nolint
  plot_ggfortify(object, ...) +
    apply_theme(list(
      scale_color_viridis_d("Feature"),
      theme_mlr3()
    ))
}

#' @export
plot.LearnerClassifCVGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
