#' @title Plot for Classification Learners
#'
#' @description
#' Visualizations for [mlr3::LearnerClassif].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"prediction"` (default): Decision boundary of the learner and the true class labels.
#'
#' @param object ([mlr3::LearnerClassif]).
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
#' task = tsk("pima")$select(c("age", "pedigree"))
#' learner = lrn("classif.rpart", predict_type = "prob")
#' learner$train(task)
#'
#' autoplot(learner, type = "prediction", task)
#' }
#' }
autoplot.LearnerClassif = function(object, type = "prediction", task, grid_points = 100L, expand_range = 0, theme = theme_minimal(), ...) { # nolint
  assert_string(type)

  switch(type,
    "prediction" = {
      mlr3::assert_task(task)
      features = task$feature_names

      if (length(features) != 2L) {
        mlr3misc::stopf("Plot learner prediction only works for tasks with two features for classification!", wrap = TRUE)
      }

      grid = predict_grid(list(object), task, grid_points = grid_points, expand_range = expand_range)

      if (object$predict_type == "prob") {
        # classif, probs
        raster_aes = aes(
          fill = .data[["response"]],
          alpha = .data[[".prob.response"]])
        scale_alpha = scale_alpha_continuous(
          name = "Probability",
          guide = guide_legend(override.aes = list(fill = viridis::viridis(1))))
        scale_fill = scale_fill_viridis_d(end = 0.8)
        guides = NULL
      } else if (object$predict_type == "response") {
        # classif, no probs
        raster_aes = aes(fill = .data[["response"]])
        scale_alpha = NULL
        scale_fill = scale_fill_viridis_d(end = 0.8)
        guides = NULL
      }

      ggplot(grid,
        mapping = aes(
          x = .data[[features[1L]]],
          y = .data[[features[2L]]])) +
        geom_raster(raster_aes) +
        geom_point(
          mapping = aes(fill = .data[[task$target_names]]),
          data = task$data(),
          shape = 21,
          color = "black") +
        scale_fill +
        guides +
        theme +
        theme(legend.position = "right") +
        scale_alpha +
        labs(fill = "Response")
    },

    stopf("Unknown plot type '%s'", type)
  )
}
