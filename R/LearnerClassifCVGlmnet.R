#' @rdname autoplot.LearnerClassifGlmnet
#' @export
autoplot.LearnerClassifCVGlmnet = function(object, type = "prediction", task = NULL, grid_points = 100L, expand_range = 0, theme = theme_minimal(), ...) { # nolint
  switch(type,
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
plot.LearnerClassifCVGlmnet = function(x, ...) {
  print(autoplot(x, ...))
}
