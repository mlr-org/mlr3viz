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

plot_precrec = function(object, curvetype = "ROC", cb_alpha = 0.05, ...) {
  require_namespaces("precrec")
  x = as_precrec(object)
  dsids = unique(map_int(x, attr, which = "dsid", exact = TRUE))
  if (length(dsids) > 1L) {
    show_cb = calc_avg = TRUE
  } else {
    show_cb = calc_avg = FALSE
    cb_alpha = NULL
  }

  autoplot(precrec::evalmod(x, show_cb = show_cb, calc_avg = calc_avg, cb_alpha = cb_alpha), curvetype = curvetype)
}
