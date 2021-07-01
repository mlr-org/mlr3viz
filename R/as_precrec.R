#' @title Convert to 'precrec' Format
#'
#' @description
#' Converts to a format which is understood by [precrec::evalmod()] of
#' package \CRANpkg{precrec}.
#'
#' @param object (`any`)\cr
#'   Object to convert.
#' @return Object as created by [precrec::mmdata()].
#'
#' @references
#' `r format_bib("precrec")`
#' @export
as_precrec = function(object) {
  UseMethod("as_precrec")
}


roc_data = function(prediction) {
  prediction = mlr3::as_prediction(prediction)
  if (nlevels(prediction$truth) != 2L) {
    stopf("Need a binary classification problem to plot a ROC curve")
  }

  if ("prob" %nin% prediction$predict_types) {
    stopf("Need predicted probabilities to plot a ROC curve")
  }

  data.table(
    scores = prediction$prob[, 1L],
    labels = prediction$truth
  )
}


#' @rdname as_precrec
#' @export
as_precrec.PredictionClassif = function(object) { # nolint
  require_namespaces("precrec")
  data = roc_data(object)
  precrec::mmdata(
    scores = data$scores,
    labels = data$labels,
    dsids = 1L,
    posclass = levels(data$labels)[1L]
  )
}


#' @rdname as_precrec
#' @export
as_precrec.ResampleResult = function(object) { # nolint
  require_namespaces("precrec")
  predictions = object$predictions()
  data = transpose_list(map(predictions, roc_data))
  precrec::mmdata(
    scores = data$scores, labels = data$labels,
    dsids = seq_along(predictions),
    posclass = levels(data$labels)[1L]
  )
}


#' @rdname as_precrec
#' @export
as_precrec.BenchmarkResult = function(object) { # nolint
  require_namespaces("precrec")
  scores = object$score(measures = list())

  if (uniqueN(scores$task_id) > 1L) {
    stopf("Unable to convert benchmark results with multiple tasks.")
  }
  if (uniqueN(scores$resampling_id) > 1L) {
    stopf("Unable to convert benchmark results with multiple resamplings.")
  }

  predictions = scores$prediction
  data = transpose_list(map(predictions, roc_data))
  data$labels = split(data$labels, scores$iteration)
  data$scores = split(data$scores, scores$iteration)

  lrns = unique(scores$learner_id)
  iters = unique(scores$iteration)
  precrec::mmdata(
    data$scores,
    data$labels,
    dsids = iters,
    modnames = lrns,
    posclass = levels(data$labels)[1L]
  )
}
