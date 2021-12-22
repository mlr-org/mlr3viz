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

plot_precrec = function(object, curvetype = "ROC", cb_alpha = 0.05, show_cb = TRUE, calc_avg = TRUE, ...) {
  require_namespaces("precrec")
  x = as_precrec(object)
  dsids = unique(map_int(x, attr, which = "dsid", exact = TRUE))
  if (length(dsids) <= 1L) {
    show_cb = calc_avg = FALSE
    cb_alpha = NULL
  }

  autoplot(precrec::evalmod(x, calc_avg = calc_avg, cb_alpha = cb_alpha), curvetype = curvetype, show_cb = show_cb, ...)
}

delayed_patchwork = function(li, ...) {
  assert_list(li, min.len = 1L)
  attr(li, ".args") = list(...)
  class(li) = "DelayedPatchworkPlot"
  li
}

#' @export
print.DelayedPatchworkPlot = function(x, ...) {
  require_namespaces("patchwork")
  print(invoke(patchwork::wrap_plots, x, .args = list(attr(x, ".args"))))
}

apply_theme = function(theme_object, default_object = NULL) {
  if (getOption("mlr3.theme", TRUE)) theme_object else default_object %??% geom_blank()
}
