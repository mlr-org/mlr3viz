#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerRegrGlmnet = function(object, ...) { # nolint
  plot_ggfortify(object, ...)
}

#' @export
plot.LearnerRegrGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
