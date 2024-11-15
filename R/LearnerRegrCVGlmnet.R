#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerRegrCVGlmnet = function(object, type = "prediction", task = NULL, grid_points = 100L, expand_range = 0, theme = theme_minimal(), ...) { # nolint
  assert_choice(type, choices = c("prediction", "ggfortify"), null.ok = FALSE)
  switch(type,
    "prediction" = {
      NextMethod()
    },

    "ggfortify" = {
      plot_ggfortify(object, ...) +
        scale_color_viridis_d("Feature", end = 0.8) +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.LearnerRegrCVGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
