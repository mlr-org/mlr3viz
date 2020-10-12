plot_ggfortify = function(object, ...) {
  assert_has_model(object)
  require_namespaces("ggfortify")
  autoplot(object$model, ...)
}

assert_has_model = function(learner) {
  if (is.null(learner$model)) {
    stopf("Learner '%s' must be trained first", learner$id)
  }
}
