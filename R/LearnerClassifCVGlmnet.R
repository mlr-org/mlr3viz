#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerClassifCVGlmnet = function(object, theme = theme_minimal(), ...) { # nolint
  plot_ggfortify(object, ...) +
    scale_color_viridis_d("Feature") +
    theme

}

#' @export
plot.LearnerClassifCVGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
