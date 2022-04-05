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
#' * `"preds"`: Matplots the survival curves for all predictions
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
#' @template section_theme
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
#'
#' # Predictions
#' autoplot(p, type = "preds")
autoplot.PredictionSurv = function(object, type = "dcalib",
  task = NULL, row_ids = NULL, times = NULL, xyline = TRUE,
  cuts = 11L, ...) {

  assert("distr" %in% object$predict_types)

  switch(type,
    "calib" = {
      if (is.null(times)) {
        times = sort(unique(task$truth()[, 1]))
      }

      if (inherits(object$distr, "VectorDistribution")) {
        pred_surv = 1 - distr6::as.MixtureDistribution(object$distr)$cdf(times)
      } else {
        pred_surv = colMeans(1 - object$distr$cdf(times))
      }

      km = mlr3::lrn("surv.kaplan")
      km_pred = km$train(task, row_ids = row_ids)$predict(task, row_ids = row_ids)
      km_surv = colMeans(1 - km_pred$distr$cdf(times))

      data = data.frame(x = times, y = c(km_surv, pred_surv),
        Group = rep(c("KM", "Pred"), each = length(times)))

      ggplot(data, aes(x = .data[["x"]], y = .data[["y"]], group = .data[["Group"]], color = .data[["Group"]])) +
        geom_line() +
        labs(x = "T", y = "S(T)") +
        apply_theme(list(
          scale_color_viridis_d(end = 0.8),
          theme_mlr3()
        )) +
        theme(legend.title = element_blank())

    },

    "dcalib" = {
      p = seq.int(0, 1, length.out = cuts)
      q = map_dbl(p, function(.x) {
        sum(object$truth[, 1L] <= as.numeric(object$distr$quantile(.x))) / length(object$row_ids)
      })
      pl = qplot(x = p, y = q, geom = "line")
      if (xyline) {
        pl = pl + geom_abline(slope = 1, intercept = 0, color = "lightgray")
      }
      pl +
        labs(x = "True", y = "Predicted") +
        apply_theme(list(theme_mlr3()))
    },

    "preds" = {
      v = 1 - distr6::gprm(object$distr, "cdf")
      surv = data.frame(
        Var1 = as.factor(seq_len(nrow(v))),
        Var2 = rep(as.numeric(colnames(v)), each = nrow(v)),
        value = invoke(c, .args = as.data.frame(v))
      )

      ggplot(surv, aes(x = .data[["Var2"]], y = .data[["value"]], group = .data[["Var1"]], color = .data[["Var1"]])) +
        geom_line() +
        labs(x = "T", y = "S(T)") +
        apply_theme(list(theme_mlr3())) +
        theme(legend.position = "n")
    },

    stopf("Unknown plot type '%s'", type)
  )
}
