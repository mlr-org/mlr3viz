#' @rdname autoplot.LearnerClassifGlmnet
#' @export
#nolint next
autoplot.LearnerRegrGlmnet = function(
  object,
  type = "prediction",
  task = NULL,
  grid_points = 100L,
  expand_range = 0,
  theme = theme_minimal(),
  ...
) {
  assert_has_model(object)

  switch(
    type,
    "prediction" = {
      NextMethod()
    },

    "ggfortify" = {
      plot_ggfortify(object, ...) +
        scale_color_viridis_d("Feature") +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.LearnerRegrGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
