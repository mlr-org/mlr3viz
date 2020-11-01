#' @title Plot for PredictionSurv
#'
#' @description
#' Generates plots for [mlr3proba::PredictionSurv], depending on argument `type`:
#'
#' * `"calib"` (default): Calibration plot comparing the average predicted survival distribution
#'   to a Kaplan-Meier prediction, this is *not* a comparison of a stratified `crank` or `lp`
#'   prediction. `object` must have `distr` prediction. `geom_line()` is used for comparison split
#'   between the prediction (`Pred`) and Kaplan-Meier estimate (`KM`). In addition labels are added
#'   for the x (`T`) and y (`S(T)`) axes.
#'
#' @param object ([mlr3proba::PredictionSurv]).
#' @template param_type
#' @param task ([mlr3proba::TaskSurv]) \cr
#'   If `type = "calib"` then `task` is passed to `$predict` in the Kaplan-Meier learner.
#' @param row_ids (`integer()`) \cr
#'   If `type = "calib"` then `row_ids` is passed to `$predict` in the Kaplan-Meier learner.
#' @param times (`numeric()`) \cr
#'   If `type = "calib"` then `times` is the values on the x-axis to plot over,
#'    if `NULL` uses all times from `task`.
#' @param ... (`any`):
#'   Additional arguments, currently unused.
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' library(mlr3viz)
#'
#' learn = lrn("surv.coxph")
#' task = tsk("rats")
#' p = learn$train(task, row_ids = 1:100)$predict(task, row_ids = 101:200)
#' autoplot(p, type = "calib", task = task)
autoplot.PredictionSurv = function(object, type = "calib", task = NULL, row_ids = NULL, times = NULL, ...) { # nolint
  x = y = Group = NULL

  switch(type,
    "calib" = {
      assert("distr" %in% object$predict_types)
      pred_distr = distr6::as.MixtureDistribution(object$distr)

      km = mlr3::lrn("surv.kaplan")
      km_pred = km$train(task, row_ids = row_ids)$predict(task, row_ids = row_ids)
      km_distr = distr6::as.MixtureDistribution(km_pred$distr)

      if (is.null(times)) {
        times = sort(unique(task$truth()[, 1]))
      }

      data = data.frame(x = times, y = c(1 - km_distr$cdf(times), 1 - pred_distr$cdf(times)),
                        Group = rep(c("KM", "Pred"), each = length(times)))

      ggplot(data, aes(x = x, y = y, group = Group, color = Group)) + geom_line() +
        labs(x = "T", y = "S(T)")
    },

    stopf("Unknown plot type '%s'", type)
  )
}
