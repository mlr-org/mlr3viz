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
#' * `"dcalib"`: Distribution calibration plot. A model is D-calibrated if X% of deaths occur before
#'   the X/100 quantile of the predicted distribution, e.g. if 50% of observations die before their
#'   predicted median survival time. A model is D-calibrated if the resulting plot lies on x = y.
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
#' @param xyline (`logical(1)`) \cr
#'   If `TRUE` (default) plots the x-y line for `type = "dcalib"`.
#' @param cuts (`integer(1)`) \cr
#'   Number of cuts in (0,1) to plot `dcalib` over, default is `11`.
#' @param ... (`any`):
#'   Additional arguments, currently unused.
#'
#' @references
#' `r format_bib("dcalib")`
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' library(mlr3viz)
#'
#' learn = lrn("surv.coxph")
#' task = tsk("unemployment")
#' p = learn$train(task, row_ids = 1:300)$predict(task, row_ids = 301:400)
#'
#' # calibration by comparison of average prediction to Kaplan-Meier
#' autoplot(p, type = "calib", task = task, row_ids = 301:400)
#'
#' # Distribution-calibration (D-Calibration)
#' autoplot(p, type = "dcalib")
autoplot.PredictionSurv = function(object, type = c("calib", "dcalib"),
  task = NULL, row_ids = NULL, times = NULL, xyline = TRUE,
  cuts = 11L, ...) {

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

    "dcalib" = {
      p = seq.int(0, 1, length.out = cuts)
      q = map_dbl(p, function(.x) {
        sum(object$truth[, 1L] <= as.numeric(object$distr$quantile(.x))) / length(object$row_ids)
      })
      pl = qplot(x = p, y = q, geom = "line")
      if (xyline) {
        pl = pl + geom_abline(slope = 1, intercept = 0, color = "red")
      }
      pl + labs(x = "True", y = "Predicted")
    },

    stopf("Unknown plot type '%s'", type)
  )
}
