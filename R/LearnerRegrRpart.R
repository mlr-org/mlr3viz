#' @export
#' @rdname autoplot.LearnerClassifRpart
autoplot.LearnerRegrRpart = function(object, ...) { # nolint
  if (is.null(object$model)) {
    stopf("Learner '%s' must be trained first", object$id)
  }
  if (is.null(object$model$model)) {
    stopf("Learner '%s' must be trained with `keep_model` set to `TRUE`", object$id)
  }
  require_namespaces(c("partykit", "ggparty"))

  target = all.vars(object$model$terms)[1L]
  autoplot(partykit::as.party(object$model), ...) +
    ggparty::geom_node_plot(gglist = list(
        geom_boxplot(aes_string(target)),
        coord_flip(),
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    ))
}
