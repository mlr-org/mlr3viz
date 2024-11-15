#' @title Plots for Resample Results
#'
#' @description
#' Visualizations for [mlr3::ResampleResult].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#' * `"boxplot"` (default): Boxplot of performance measures.
#' * `"histogram"`: Histogram of performance measures.
#' * `"roc"`: ROC curve (1 - specificity on x, sensitivity on y).
#'    The predictions of the individual [mlr3::Resampling]s are merged prior to calculating the ROC curve (micro averaged).
#'    Requires package \CRANpkg{precrec}.
#' * `"prc"`: Precision recall curve.
#'    See `"roc"`.
#' * `"prediction"`: Plots the learner prediction for a grid of points.
#'    Needs models to be stored. Set `store_models = TRUE` for [mlr3::resample()].
#'    For classification, we support tasks with exactly two features and learners with `predict_type=` set to `"response"` or `"prob"`.
#'    For regression, we support tasks with one or two features.
#'    For tasks with one feature we can print confidence bounds if the predict type of the learner was set to `"se"`.
#'    For tasks with two features the predict type will be ignored.
#'
#' @param object ([mlr3::ResampleResult]).
#' @template param_type
#' @template param_measure
#' @param predict_sets (`character()`)\cr
#'  Only for `type` set to `"prediction"`.
#'  Which points should be shown in the plot?
#'  Can be a subset of (`"train"`, `"test"`) or empty.
#' @param binwidth (`integer(1)`)\cr
#'  Width of the bins for the histogram.
#' @template param_theme
#' @param ... arguments passed on to [precrec::autoplot()] for `type = "roc"` or `"prc"`.
#' Useful to e.g. remove confidence bands with `show_cb = FALSE`.
#'
#' @return [ggplot2::ggplot()].
#'
#' @references
#' `r format_bib("precrec")`
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3viz)
#'
#'   task = tsk("sonar")
#'   learner = lrn("classif.rpart", predict_type = "prob")
#'   resampling = rsmp("cv", folds = 3)
#'   object = resample(task, learner, resampling)
#'
#'   head(fortify(object))
#'
#'   # Default: boxplot
#'   autoplot(object)
#'
#'   # Histogram
#'   autoplot(object, type = "histogram", bins = 30)
#'
#'   # ROC curve, averaged over resampling folds:
#'   autoplot(object, type = "roc")
#'
#'   # ROC curve of joint prediction object:
#'   autoplot(object$prediction(), type = "roc")
#'
#'   # Precision Recall Curve
#'   autoplot(object, type = "prc")
#'
#'   # Prediction Plot
#'   task = tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))
#'   resampling = rsmp("cv", folds = 3)
#'   object = resample(task, learner, resampling, store_models = TRUE)
#'   autoplot(object, type = "prediction")
#' }
#' }
autoplot.ResampleResult = function(object, type = "boxplot", measure = NULL, predict_sets = "test", binwidth = NULL, theme = theme_minimal(), ...) {
  assert_choice(type, choices = c("boxplot", "histogram", "prediction", "roc", "prc"), null.ok = FALSE)

  task = object$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)

  switch(type,
    "boxplot" = {
      ggplot(object,
        measure = measure,
        mapping = aes(y = .data[["performance"]])) +
        geom_boxplot(
          fill = viridis::viridis(1, begin = 0.5),
          alpha = 0.8,
          show.legend = FALSE) +
        scale_x_discrete() +
        ylab(measure$id) +
        theme +
        theme(axis.text.x.bottom = element_blank())
    },

    "histogram" = {
      ggplot(object,
        measure = measure,
        aes(x = .data[["performance"]])) +
        geom_histogram(
          fill = viridis::viridis(1, begin = 0.5),
          alpha = 0.8,
          color = "black",
          binwidth = binwidth) +
        xlab(measure$id) +
        ylab("Count") +
        theme
    },

    "roc" = {
      p = plot_precrec(object, curvetype = "ROC", ...)
      p$layers[[1]]$mapping = aes(color = modname, fill = modname)
      # fill confidence bounds
      p +
        guides(
          color = "none",
          fill = "none") +
        scale_color_viridis_d("Learner", begin = 0.5) +
        scale_fill_viridis_d("Learner", begin = 0.5) +
        theme +
        theme(plot.title = element_blank(), legend.position = "none")
    },

    "prc" = {
      p = plot_precrec(object, curvetype = "PRC", ...)
      # fill confidence bounds
      p$layers[[1]]$mapping = aes(color = modname, fill = modname)
      p +
        guides(
          color = "none",
          fill = "none") +
        scale_color_viridis_d("Learner", begin = 0.5) +
        scale_fill_viridis_d("Learner", begin = 0.5) +
        theme +
        theme(plot.title = element_blank())
    },

    "prediction" = plot_learner_prediction_resample_result(object, predict_sets, theme = theme, ...),

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
plot.ResampleResult = function(x, ...) {
  print(autoplot(x, ...))
}

#' @export
fortify.ResampleResult = function(model, data, measure = NULL, ...) { # nolint
  task = model$task
  measure = mlr3::assert_measure(mlr3::as_measure(measure, task_type = task$task_type), task = task)
  data = model$score(measure)[, c("iteration", measure$id), with = FALSE]
  melt(data,
    measure.vars = measure$id,
    variable.name = "measure_id", value.name = "performance")
}

plot_learner_prediction_resample_result = function(object, predict_sets, grid_points = 100L, expand_range = 0, theme = theme_minimal()) {

  task = object$task
  task_type = task$task_type
  features = task$feature_names

  dim = length(features)
  learners = object$learners

  if (any(map_lgl(map(learners, "model"), is.null))) {
    mlr3misc::stopf("No trained models available. Set 'store_models = TRUE' in 'resample()'.", wrap = TRUE)
  }

  if (task_type %nin% c("classif", "regr")) {
    stopf("Unsupported task type: %s", task_type)
  }
  if (task_type == "classif" && dim != 2L) {
    mlr3misc::stopf("Plot learner prediction only works for tasks with two features for classification!", wrap = TRUE)
  }
  if (task_type == "regr" && dim %nin% 1:2) {
    mlr3misc::stopf("Plot learner prediction only works with one or two features for regression!", wrap = TRUE)
  }

  grid = predict_grid(learners, task, grid_points = grid_points,  expand_range = expand_range)

  # facets for multiple resampling iterations
  if (length(learners) > 1L) {
    iters = seq_len(object$resampling$iters)
    facet_labels = paste(object$resampling$id, iters)
    names(facet_labels) = iters
    folds_facet = facet_wrap(".id", labeller = as_labeller(facet_labels))
  } else {
    folds_facet = NULL
  }

  # 1d plot (only regression)
  if (task_type == "regr" && dim == 1L) {
    if (learners[[1L]]$predict_type == "se") {
      se_geom = geom_ribbon(
          mapping = aes(
            ymin = .data[["response"]] - .data[["se"]],
            ymax = .data[["response"]] + .data[["se"]]),
          alpha = 0.2,
          fill = viridis::viridis(1, begin = 0.5))
    } else {
      se_geom = NULL
    }

    g = ggplot(grid,
      mapping = aes(
        x = .data[[features]],
        y = .data[["response"]])) +
      se_geom +
      geom_line(color = viridis::viridis(1, begin = 0.5)) +
      geom_point(data = task_data(object, predict_sets),
        mapping = aes(
          y = .data[[task$target_names]],
          shape = .data[[".predict_set"]],
          color = .data[[".predict_set"]])) +
      scale_shape_manual(
        values = c(train = 16, test = 15, both = 17),
        name = "Set") +
      labs(color = "Set") +
      scale_color_viridis_d(end = 0.8) +
      theme +
      folds_facet

    # 2d plot regr + classif
  } else if (dim == 2L) {
    if (task_type == "classif" && learners[[1L]]$predict_type == "prob") {
      # classif, probs
      raster_aes = aes(
        fill = .data[["response"]],
        alpha = .data[[".prob.response"]])
      scale_alpha = scale_alpha_continuous(
        name = "Probability",
        guide = guide_legend(override.aes = list(fill = viridis::viridis(1))))
      scale_fill = scale_fill_viridis_d(end = 0.8)
      guides = NULL
    } else if (task_type == "classif" && learners[[1L]]$predict_type == "response") {
      # classif, no probs
      raster_aes = aes(fill = .data[["response"]])
      scale_alpha = NULL
      scale_fill = scale_fill_viridis_d(end = 0.8)
      guides = NULL
    } else {
      # regr
      raster_aes = aes(fill = .data[["response"]])
      scale_alpha = NULL
      scale_fill = scale_fill_viridis_c(end = 0.8)
      guides = guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10))
    }

    if (!is.numeric(grid[[features[1L]]])) {
      theme = theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    g = ggplot(grid,
      mapping = aes(
        x = .data[[features[1L]]],
        y = .data[[features[2L]]])) +
      geom_raster(raster_aes) +
      geom_point(
        mapping = aes(fill = .data[[task$target_names]], shape = .data[[".predict_set"]]),
        data = task_data(object, predict_sets),
        color = "black") +
      scale_fill +
      guides +
      theme +
      theme(legend.position = "right") +
      scale_shape_manual(
        values = c(train = 21, test = 22, both = 23),
        name = "Set") +
      scale_alpha +
      labs(fill = "Response") +
      folds_facet
  }

  return(g)
}

# Generates a data.table that contains the points that were used in each
# resample iterations.
# The resample iteration is given in column ".id"
# The point type is given in column ".predict_set"
# object: ResampleResult
# predict_sets: see above
task_data = function(object, predict_sets) {

  # if train and test is in predict_sets, allow "both" to be plotted

  if (all(c("train", "test") %in% predict_sets) && "both" %nin% predict_sets) {
    predict_sets = c(predict_sets, "both")
  }
  type_char = c("none", "train", "test", "both")
  # set all unwanted point types to NA
  type_char[type_char %nin% predict_sets] = NA_character_

  types = lapply(seq_along(object$learners), function(i) {
    ids = seq_len(object$task$nrow)
    type = (ids %in% object$resampling$train_set(i)) + 2L *
      (ids %in% object$resampling$test_set(i))
    type = type_char[type + 1L]
    select_ids = !is.na(type)
    data.table(.row_id = ids[select_ids], .predict_set = type[select_ids])
  })

  types = rbindlist(types, idcol = TRUE, use.names = FALSE)
  data = cbind(types, object$task$data()[types$.row_id, ])
  return(remove_named(data, ".row_id"))
}

# Generates a evenly distributed sequence of the same type as the input vector.
# x: vector of any type (column of a task)
# n (int): desired resolution
# expand_range (num): expand the outer limits (only for numerics)
# result: vector(n) of the same type as x
sequenize = function(x, n, expand_range = 0) {
  if (is.numeric(x)) {
    r = range(x, na.rm = TRUE)
    d = diff(r)
    res = seq(
      from = r[1L] - expand_range * d, to = r[2L] + expand_range * d,
      length.out = n)
    if (is.integer(x)) {
      res = unique(as.integer(round(res)))
    }
  } else if (is.factor(x) || is.ordered(x)) {
    # keeps the order of the levels
    res = factor(levels(x), levels = levels(x), ordered = is.ordered(x))
  } else if (is.logical(x)) {
    res = c(TRUE, FALSE)
  } else {
    stopf("Type of column (%s) not supported.", typeof(x))
  }
  return(res)
}

#' @title Generates a data.table of evenly distributed points.
#' @description
#' For each point we have the predicted class / regression value  in column
#' response. If the learner predicts probabilities, a column ".prob.response" is
#' added that contains the probability of the predicted class
#'
#' @param learners list of trained learners, each learner belongs to one
#'   resampling iteration
#' @param task the task all learners are trained on
#' @param grid_points (int): see sequenize
#' @param expand_range see sequenize
predict_grid = function(learners, task, grid_points, expand_range) {
  grids = mlr3misc::map(learners, function(learner) {
    features = task$feature_names
    grid = mlr3misc::map(task$data(cols = features), sequenize, n = grid_points, expand_range = expand_range)
    grid = cross_join(grid, sorted = FALSE)
    grid = cbind(
      grid,
      remove_named(as.data.table(learner$predict_newdata(
        newdata = grid,
        task = task)), c("row_id", "truth")))
  })
  grid = rbindlist(grids, idcol = TRUE, use.names = FALSE)

  # reduce to prob columns to one column for the predicted class
  if (learners[[1]]$predict_type == "prob") {
    grid[, ".prob.response" := .SD[, paste0(
      "prob.", # nolint
      get("response")), with = FALSE], by = "response"]
  }

  return(grid)
}
