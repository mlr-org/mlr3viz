#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerClassifCVGlmnet = function(object, ...) { # nolint
  plot_ggfortify(object, ...)
}

#' @export
plot.LearnerClassifCVGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
