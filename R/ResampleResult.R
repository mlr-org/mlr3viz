#' @title Plot for ResampleResult
#'
#' @description
#' Generates plots for [mlr3::ResampleResult], depending on argument `type`:
#' * `"boxplot"` (default): Boxplot of performance measures.
#' * `"histogram"`: Histogram of performance measures.
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'   The predictions of the individual [mlr3::Resampling]s are merged prior to calculating the ROC curve
#'   (micro averaged). Requires package \CRANpkg{precrec}.
#' * `"prc"`: Precision recall curve. See `"roc"`.
#'
#' @param object ([mlr3::ResampleResult]).
#' @template param_type
#' @param measure ([mlr3::Measure]).
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("sonar")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' resampling = rsmp("cv")
#' object = resample(task, learner, resampling)
#'
#' head(fortify(object))
#'
#' # Default: boxplot
#' autoplot(object)
#'
#' # Histogram
#' autoplot(object, type = "histogram", bins = 30)
#'
#' # ROC curve, averaged over resampling folds:
#' autoplot(object, type = "roc")
#'
#' # ROC curve of joint prediction object:
#' autoplot(object$prediction(), type = "roc")
#'
#' # Precision Recall Curve
#' autoplot(object, type = "prc")
autoplot.ResampleResult = function(object, type = "boxplot", measure = NULL, ...) {
  assert_string(type)

  task = object$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)

  switch(type,
    "boxplot" = {
      ggplot(object, measure = measure, aes_string(y = "performance")) + geom_boxplot(...) + ylab(measure$id)
    },

    "histogram" = {
      ggplot(object, measure = measure, aes_string(x = "performance")) + geom_histogram(...) + xlab(measure$id)
    },

    "roc" = {
      require_namespaces("precrec")
      autoplot(precrec::evalmod(as_precrec(object)), curvetype = "ROC", show_cb = TRUE)
    },

    "prc" = {
      require_namespaces("precrec")
      autoplot(precrec::evalmod(as_precrec(object)), curvetype = "prc", show_cb = TRUE)
    },

    "prediction" = plot_learner_prediction_resample_result(object$learners[[1]], object, measure, ...),

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
fortify.ResampleResult = function(model, data, measure = NULL, ...) {
  task = model$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  data = model$score(measure)[, c("iteration", measure$id), with = FALSE]
  melt(data, measure.vars = measure$id,
    variable.name = "measure_id", value.name = "performance")
}

plot_learner_prediction_resample_result = function(learner, object, measure, grid_points = 100L, expand_range = 0) {
  UseMethod("plot_learner_prediction_resample_result")
}

plot_learner_prediction_resample_result.LearnerClassif = function(learner, object, measure, grid_points = 100L, expand_range = 0) {

  task = object$task
  features = task$feature_names
  learners = object$learners

  if (length(features) != 2) {
    stop("plot_learner_prediction.TaskClassif only works for tasks with two features!")
  }

  grid_res = predict_grid(learners, task, grid_points = grid_points, expand_range = expand_range)

  if (length(learners) > 1) {
    facet_labels = setNames(paste(object$resampling$id, seq_len(object$resampling$iters)), seq_len(object$resampling$iters))
    folds_facet = facet_wrap(".id", labeller = as_labeller(facet_labels))
    folds_facet = facet_wrap(".id")
  } else {
    folds_facet = NULL
  }

  if (learner$predict_type == "prob") {
    grid_res[, "prob.response" := .SD[, paste0("prob.", get("response")), with = FALSE] , by = "response"]
    raster_aes = aes_string(fill = "response", alpha = "prob.response")
  } else {
    raster_aes = aes_string(fill = "response")
  }

  ggplot(grid_res, aes_string(features[1L], features[2L])) +
    geom_raster(raster_aes) +
    geom_point(data = task_data(object), aes_string(features[1L], features[2L], fill = task$target_names, shape = ".point_type"), size = 4, color = "black") +
    scale_shape_manual(values = c(train = 21, test = 22, both = 23), name = "set") +
    folds_facet
}

plot_learner_prediction_resample_result.LearnerRegr = function(learner, object, measure, grid_points = 100L, expand_range = 0) {

  task = object$task
  features = task$feature_names
  learners = object$learners

  if (length(features) > 2) {
    stop("plot_learner_prediction.LearnerRegr only works with one or two features!")
  }

  grid = predict_grid(learners, task, grid_points, expand_range)

  if (length(learners) > 1) {
    facet_labels = setNames(paste(object$resampling$id, seq_len(object$resampling$iters)), seq_len(object$resampling$iters))
    folds_facet = facet_wrap(".id", labeller = as_labeller(facet_labels))
  } else {
    folds_facet = NULL
  }

  if (length(features) == 1) {
    if (learner$predict_type == "se") {
      se_geom = geom_ribbon(aes_string(ymin = "response-se", ymax = "response+se"), alpha = 0.2)
    } else {
      se_geom = NULL
    }
    g = ggplot(grid, aes_string(features, "response")) +
      se_geom +
      geom_line() +
      geom_point(data = task_data(object), aes_string(features, task$target_names, shape = ".point_type", color = ".point_type"), size = 4) +
      scale_shape_manual(values = c(train = 16, test = 15, both = 17), name = "set") +
      folds_facet
  } else if (length(features) == 2) {
    g = ggplot(grid, aes_string(features[1L], features[2L])) +
      geom_raster(aes_string(fill = "response")) +
      geom_point(data = task_data(object), aes_string(features[1L], features[2L], fill = task$target_names, shape = ".point_type"), size = 4, color = "black", shape = 21) +
      scale_shape_manual(values = c(train = 21, test = 22, both = 23), name = "set", shape = 21) +
      folds_facet
  }

  return(g)

}

task_data = function(object) {
  data = lapply(seq_along(object$learners), function(i) {
    data = object$task$data()
    train_inds = object$resampling$train_set(i)
    test_inds = object$resampling$test_set(i)
    data[train_inds, ".point_type" := "train"]
    data[test_inds, ".point_type" := "test"]
    data[intersect(train_inds, test_inds), ".point_type" := "both"]
    return(data)
  })
  data = rbindlist(data, idcol = TRUE, use.names = FALSE)
  return(data)
}

sequenize = function(x, n, expand_range) {
  if (is.numeric(x)) {
    r = range(x, na.rm = TRUE)
    d = diff(r)
    res = seq(from = r[1L] - expand_range * d, to = r[2L] + expand_range * d, length.out = n)
    if (is.integer(x)) {
      res = unique(as.integer(round(res)))
    }
  } else if (is.factor(x) || is.ordered(x)) {
    res = factor(levels(x), levels = levels(x), ordered = is.ordered(x)) # keeps the order of the levels
  } else if (is.logical(x)) {
    res = c(TRUE, FALSE)
  } else {
    stopf("Type of column (%s) not supported.", typeof(x))
  }
  return(res)
}

predict_grid = function(learners, task, grid_points, expand_range) {
  grids = lapply(learners, function(learner) {
    features = task$feature_names
    grid = map(task$data(cols = features), sequenize, n = grid_points, expand_range = expand_range)
    grid = cross_join(grid, sorted = FALSE)
    grid = cbind(grid, remove_named(as.data.table(learner$predict_newdata(newdata = grid, task = task)), c("row_id", "truth")))
  })
  grid = rbindlist(grids, idcol = TRUE, use.names = FALSE)
  return(grid)
}



