#' @title Plot for Regression Learners
#'
#' @description
#' Visualizations for [mlr3::LearnerRegr].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"prediction"` (default): Decision boundary of the learner and the true class labels.
#'
#' @param object ([mlr3::LearnerRegr]).
#'
#' @template param_type
#' @template param_task
#' @template param_grid_points
#' @template param_expand_range
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3")) {
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("mtcars")$select(c("am", "carb"))
#' learner = lrn("regr.rpart")
#' learner$train(task)
#'
#' autoplot(learner, type = "prediction", task)
#' }
#' }
autoplot.LearnerRegr = function(object, type = "prediction", task, grid_points = 100L, expand_range = 0, theme = theme_minimal(), ...) { # nolint
  assert_string(type)

  switch(type,
    "prediction" = {
      mlr3::assert_task(task)
      features = task$feature_names

      if (length(features) > 2L) {
        mlr3misc::stopf("Plot learner prediction only works with one or two features for regression!", wrap = TRUE)
      }

      grid = predict_grid(list(object), task, grid_points = grid_points, expand_range = expand_range)

      if (length(features) == 1) {
        if (object$predict_type == "se") {
          se_geom = geom_ribbon(
            mapping = aes(
              ymin = .data[["response"]] - .data[["se"]],
              ymax = .data[["response"]] + .data[["se"]]),
            alpha = 0.2,
            fill = viridis::viridis(1, begin = 0.5))
        } else {
          se_geom = NULL
        }

        ggplot(grid,
          mapping = aes(
            x = .data[[features]],
            y = .data[["response"]])) +
          se_geom +
          geom_line(color = viridis::viridis(1, begin = 0.5)) +
          geom_point(
            data = task$data(),
            shape = 21,
            color = "black",
            mapping = aes(
              y = .data[[task$target_names]])) +
          scale_color_viridis_d(end = 0.8) +
          theme
      } else {

        if (!is.numeric(grid[[features[1L]]])) {
          theme = theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }

        ggplot(grid,
          mapping = aes(
            x = .data[[features[1L]]],
            y = .data[[features[2L]]])) +
          geom_raster(aes(fill = .data[["response"]])) +
          geom_point(
            mapping = aes(fill = .data[[task$target_names]]),
            data = task$data(),
            shape = 21,
            color = "black") +
          scale_fill_viridis_c(end = 0.8) +
          guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
          theme +
          theme(legend.position = "right") +
          labs(fill = "Response")
      }
    },

    stopf("Unknown plot type '%s'", type)
  )
}
