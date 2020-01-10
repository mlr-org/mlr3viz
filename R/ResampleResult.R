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

    "pred2D" = {
      plotpred2D(object, ...)
    },
    
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

plotpred2D = function(object, gridsize = 100, prob_alpha = TRUE,
                      pointsize = 2,
                      err_mark = "train",
                      bg_cols = c("darkblue", "green", "darkred"),
                      err_col = "white", err_size = pointsize,
                      greyscale = FALSE) {
  task = object$task
  target_names = task$target_names
  feature_names = task$feature_names
  
  data = task$data(task$row_ids)
  features = task$data(task$row_ids, cols = feature_names)
  
  x1n = feature_names[1]
  x2n = feature_names[2]
  
  x1 = features[, 1]
  x2 = features[, 2]
  
  y = task$data(task$row_ids, cols = target_names)[[1]]
  
  grid = expand.grid(
    seq(min(x1), max(x1), length.out = gridsize),
    seq(min(x2), max(x2), length.out = gridsize)
  )
  
  colnames(grid) = feature_names
  
  cmpl_lrn = object$learners[[1]]$clone()
  cmpl_lrn$train(task)
  train_pred = cmpl_lrn$predict(task)
  y_hat = train_pred$response
  
  if (task$task_type == "classif" && "prob" %in% cmpl_lrn$predict_types) {
    cmpl_lrn$predict_type = "prob"
  }
  
  grid_pred = cmpl_lrn$predict_newdata(newdata = grid)
  grid[, target_names] = grid_pred$response
  
  if (task$task_type == "classif") {
    data$.err = if (err_mark == "train") {
      y != y_hat
    } else if (err_mark == "cv") {
      y != object$prediction()$data$tab$response
    } else {
      TRUE
    }
    
    if ("prob" %in% cmpl_lrn$predict_types && prob_alpha) {
      grid$.prob.pred.class = apply(grid_pred$prob, 1, max)
      p = ggplot(grid, aes_string(x = x1n, y = x2n))
      p = p + geom_raster(
        data = grid, mapping = aes_string(fill = target_names, alpha = ".prob.pred.class"),
        show.legend = TRUE
      ) + scale_fill_discrete(drop = FALSE)
      p = p + scale_alpha(limits = range(grid$.prob.pred.class))
    } else {
      p = p + geom_raster(mapping = aes_string(fill = target_names))
    }
    
    # print normal points
    p = p + geom_point(
      data = subset(data, !data$.err),
      mapping = aes_string(x = x1n, y = x2n, shape = target_names), size = pointsize
    )
    # mark incorrect points
    if (err_mark != "none" && any(data$.err)) {
      p = p + geom_point(data = subset(data, data$.err),
                         mapping = aes_string(x = x1n, y = x2n, shape = target_names),
                         size = err_size + 1.5, show.legend = FALSE)
      p = p + geom_point(data = subset(data, data$.err),
                         mapping = aes_string(x = x1n, y = x2n, shape = target_names),
                         size = err_size + 1, col = err_col, show.legend = FALSE)
    }
    # print error points
    p = p + geom_point(data = subset(data, data$.err),
                       mapping = aes_string(x = x1n, y = x2n, shape = target_names), size = err_size, show.legend = FALSE)
    p = p + guides(alpha = FALSE)
  } else if(task$task_type == "regr"){
    # FIXME: color are not scaled correctly? can be improved?
    # plot background from model / grid
    p = ggplot(mapping = aes_string(x = x1n, y = x2n))
    p = p + geom_raster(data = grid, mapping = aes_string(fill = target_names))
    p = p + scale_fill_gradient2(low = bg_cols[1L], mid = bg_cols[2L], high = bg_cols[3L], space = "Lab")
    # plot point, with circle and interior color for y
    p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n, colour = target_names),
                       size = pointsize)
    p = p + geom_point(data = data, mapping = aes_string(x = x1n, y = x2n),
                       size = pointsize, colour = "black", shape = 1)
    # plot point, with circle and interior color for y
    p = p + scale_colour_gradient2(low = bg_cols[1L], mid = bg_cols[2L], high = bg_cols[3L], space = "Lab")
    p = p + guides(colour = FALSE)
  }
  
  # set title
  lrn.str = cln_lrn$id
  
  resamp_score = object$prediction()$score()
  train_score = train_pred$score()
  
  # title = sprintf("%s: %s", lrn.str, paramValueToString(learner$par.set, learner$par.vals))
  title = sprintf("%s", lrn.str)
  title = sprintf("%s\nTrain: %s=%s; Resampl: %s=%s", title, 
                  attr(resamp_score,"names"), as.character(round(resamp_score,2)),
                  attr(train_score,"names"), as.character(round(train_score,2)))
  
  p = p + ggtitle(title)
  
  # deal with greyscale
  if (greyscale) {
    p = p + scale_fill_grey()
  }
  return(p)
}
