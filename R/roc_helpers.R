roc_data = function(prediction) {
  lvls = levels(prediction$truth)

  if (length(lvls) != 2L) {
    stopf("Need a binary classification problem to plot a ROC curve")
  }

  if ("prob" %nin% prediction$predict_types) {
    stopf("Need predicted probabilities to plot a ROC curve")
  }

  list(
    scores = prediction$prob[, lvls[1L]],
    labels = prediction$truth,
    posclass = lvls[1L]
  )
}


autoplot_roc_bmr = function(object, task_id = NULL, curvetype = "ROC") {
  require_namespaces("precrec")
  aggr = object$aggregate(measures = list())
  if (is.null(task_id)) {
    if (uniqueN(aggr$task_id) > 1L) {
      stopf("autoplot.BenchmarkResult can only work on a benchmark results with a single task. You can select one via argument `task_id`")
    }
  } else {
    needle = assert_choice(task_id, aggr$task_id)
    aggr = aggr[list(needle), on = "task_id"]
  }

  data = transpose_list(lapply(aggr$resample_result, function(x) roc_data(x$prediction())))
  mmdata = precrec::mmdata(scores = data$scores, labels = data$labels, modnames = aggr$learner_id, posclass = data$posclass[[1L]])
  autoplot(precrec::evalmod(mmdata), curvetype = curvetype)
}
