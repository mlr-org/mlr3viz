#' @title Plots for Cox Proportional Hazards Learner
#'
#' @description
#' Visualizations for `LearnerSurvCoxPH` of `mlr3proba`.
#'
#' The argument `type` controls what kind of plot is drawn.
#' The only possible choice right now is `"ggforest"` (default) which is a
#' Forest Plot, using [ggforest][survminer::ggforest()].
#' This plot displays the estimated hazard ratios (HRs) and their confidence
#' intervals (CIs) for different variables included in the (trained) model.
#'
#' @param object (`LearnerSurvCoxPH` of `mlr3proba`).
#'
#' @template param_type
#' @param ... Additional parameters passed down to `ggforest`.
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3proba")) {
#'   library(mlr3proba)
#'   library(mlr3viz)
#'
#'   task = tsk("lung")
#'   learner = lrn("surv.coxph")
#'   learner$train(task)
#'   autoplot(learner)
#' }
#' }
autoplot.LearnerSurvCoxPH = function(object, type = "ggforest", ...) {
  assert_choice(type, choices = c("ggforest"), null.ok = FALSE)
  assert_class(object, classes = "LearnerSurvCoxPH", null.ok = FALSE)
  assert_has_model(object)

  switch(type,
    "ggforest" = {
      require_namespaces("survminer")
      suppressWarnings(survminer::ggforest(object$model, ...))
     },

     stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.LearnerSurvCoxPH = function(x, ...) {
  print(autoplot(x, ...))
}
