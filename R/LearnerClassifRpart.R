#' @title Plots for Rpart Learners
#'
#' @description
#' Visualizations for [mlr3::LearnerClassifRpart].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"prediction"` (default): Decision boundary of the learner and the true class labels.
#' * `"ggparty"`: Visualizes the tree using the package \CRANpkg{ggparty}.
#'
#' @param object ([mlr3::LearnerClassifRpart] | [mlr3::LearnerRegrRpart]).
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
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3viz)
#'
#'   # classification
#'   task = tsk("iris")
#'   learner = lrn("classif.rpart", keep_model = TRUE)
#'   learner$train(task)
#'   autoplot(learner, type = "ggparty")
#'
#'   # regression
#'   task = tsk("mtcars")
#'   learner = lrn("regr.rpart", keep_model = TRUE)
#'   learner$train(task)
#'   autoplot(learner, type = "ggparty")
#' }
autoplot.LearnerClassifRpart = function(object, type = "prediction", task = NULL, grid_points = 100L, expand_range = 0, theme = theme_minimal(), ...) { # nolint
  assert_has_model(object)

  switch(type,
    "prediction" = {
      NextMethod()
    },

    "ggparty" = {
      require_namespaces(c("partykit", "ggparty"))
      target = all.vars(object$model$terms)[1L]

      ggparty::ggparty(partykit::as.party(object$model)) +
        ggparty::geom_edge() +
        ggparty::geom_edge_label() +
        ggparty::geom_node_splitvar() +
        ggparty::geom_node_plot(
          gglist = list(
            geom_bar(aes(x = "", fill = .data[[target]]),
            alpha = 0.8,
            color = "#000000",
            linewidth = 0.5,
            position = position_fill()),
            xlab(target),
            scale_fill_viridis_d(end = 0.8),
            theme),
          ids = "terminal",
          shared_axis_labels= TRUE) +
        ggparty::geom_node_label(
          mapping = aes(label = paste0("n=", .data[["nodesize"]])),
          nudge_y = 0.03,
          ids = "terminal")
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.LearnerClassifRpart = function(x, ...) {
  print(autoplot(x, ...))
}
