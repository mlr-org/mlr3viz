#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerRegrCVGlmnet = function(object, ...) { # nolint
  plot_ggfortify(object, ...)
}

#' @export
plot.LearnerRegrCVGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
