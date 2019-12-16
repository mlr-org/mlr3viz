#' @title Convert to 'precrec' Format
#'
#' @description
#' Converts to a format which is understood by [precrec::evalmod()] of package \CRANpkg{precrec}.
#'
#' @param `object` :: `any`\cr
#'   Object to convert.
#' @return `list()` or nested `list()`.
#' @export
as_precrec = function(object) {
  UseMethod("as_precrec")
}


roc_data = function(prediction) {
  if (nlevels(prediction$truth) != 2L) {
    stopf("Need a binary classification problem to plot a ROC curve")
  }

  if ("prob" %nin% prediction$predict_types) {
    stopf("Need predicted probabilities to plot a ROC curve")
  }

  data.table(
    scores = prediction$prob[, 2L],
    labels = prediction$truth
  )
}

#' @rdname as_precrec
#' @export
as_precrec.PredictionClassif = function(object) {
  require_namespaces("precrec")
  data = roc_data(prediction)
  precrec::mmdata(scores = data$scores, labels = data$labels)
}

#' @rdname as_precrec
#' @export
as_precrec.ResampleResult = function(object) {
  require_namespaces("precrec")
  predictions = object$predictions()
  data = transpose_list(map(predictions, roc_data))
  precrec::mmdata(scores = data$scores, labels = data$labels, dsids = seq_along(predictions))
}

#' @rdname as_precrec
#' @export
as_precrec.BenchmarkResult = function(object) {
  require_namespaces("precrec")
  scores = object$score(measures = list())
  task_id = "spam"
  if (is.null(task_id)) {
    if (uniqueN(aggr$task_id) > 1L) {
      stopf("autoplot.BenchmarkResult can only work on a benchmark results with a single task. You can select one via argument `task_id`")
    }
  } else {
    needle = assert_choice(task_id, scores$task_id)
    scores = scores[list(needle), on = "task_id"]
  }

  predictions = map(scores$prediction, "test")
  data = transpose_list(map(predictions, roc_data))
  data$labels = split(data$labels, scores$iteration)
  data$scores = split(data$scores, scores$iteration)

  mmdata = precrec::mmdata(data$scores, data$labels, dsids = 1:10, modnames = unique(scores$learner_id))

  autoplot(precrec::evalmod(mmdata))
}

