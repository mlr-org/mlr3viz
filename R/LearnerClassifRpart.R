#' @title Plot for LearnerClassifRpart / LearnerRegrRpart
#'
#' @description
#' Visualize trees for [mlr3::mlr_learners_classif.rpart] and
#' [mlr3::mlr_learners_regr.rpart] using the package \CRANpkg{ggparty}.
#'
#' Contrary to \CRANpkg{ggparty}, boxplots are shown in the terminal nodes for
#' regression trees.
#'
#' Note that learner-specific plots are experimental and subject to change.
#'
#' @param object ([mlr3::LearnerClassifRpart] | [mlr3::LearnerRegrRpart]).
#' @param ... (`any`):
#'   Additional arguments, passed down to [ggparty::autoplot.party()].
#'
#' @return [ggplot2::ggplot()] object.
#'
#' @template section_theme
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
#'   autoplot(learner)
#'
#'   # regression
#'   task = tsk("mtcars")
#'   learner = lrn("regr.rpart", keep_model = TRUE)
#'   learner$train(task)
#'   autoplot(learner)
#' }
autoplot.LearnerClassifRpart = function(object, ...) { # nolint
  assert_has_model(object)

  if (is.null(object$model$model)) {
    stopf("Learner '%s' must be trained with `keep_model` set to `TRUE`", object$id)
  }

  require_namespaces(c("partykit", "ggparty"))
  target = all.vars(object$model$terms)[1L]

  ggparty::ggparty(partykit::as.party(object$model)) +
  ggparty::geom_edge() +
  ggparty::geom_edge_label() +
  ggparty::geom_node_splitvar() +
  ggparty::geom_node_plot(gglist = list(
    geom_bar(aes(x = "", fill = .data[[target]]),
      alpha = 0.8,
      color = "#000000",
      position = position_fill()),
      xlab(target),
      apply_theme(list(theme_mlr3(), scale_fill_viridis_d(end = 0.8)))),
      ids = "terminal",
      shared_axis_labels= TRUE) +
  ggparty::geom_node_label(aes(label = paste0("n=", .data[["nodesize"]])), nudge_y = 0.03, ids = "terminal")
}

#' @export
plot.LearnerClassifRpart = function(x, ...) {
  print(autoplot(x, ...))
}
