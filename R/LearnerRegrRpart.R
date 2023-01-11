#' @export
#' @rdname autoplot.LearnerClassifRpart
autoplot.LearnerRegrRpart = function(object, theme = theme_minimal(), ...) { # nolint
  if (is.null(object$model)) {
    stopf("Learner '%s' must be trained first", object$id)
  }
  if (is.null(object$model$model)) {
    stopf("Learner '%s' must be trained with `keep_model` set to `TRUE`", object$id)
  }

  require_namespaces(c("partykit", "ggparty"))
  target = all.vars(object$model$terms)[1L]

  ggparty::ggparty(partykit::as.party(object$model)) +
    ggparty::geom_edge() +
    ggparty::geom_edge_label() +
    ggparty::geom_node_splitvar() +
    ggparty::geom_node_plot(
      gglist = list(
        geom_boxplot(aes(y = .data[[target]]),
          fill = viridis::viridis(1, begin = 0.5),
          alpha = 0.8,
          color = "#000000",
          linewidth = 0.5),
        scale_x_discrete(),
        theme,
        theme(
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()
        ))) +
    ggparty::geom_node_label(
      aes(label = paste0("n=", .data[["nodesize"]])),
      nudge_y = 0.03,
      ids = "terminal")
}

#' @export
plot.LearnerRegrRpart = function(x, ...) {
  print(autoplot(x, ...))
}
