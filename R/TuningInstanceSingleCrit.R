#' @title Plot for TuningInstanceSingleCrit
#'
#' @description
#' Generates plots for [mlr3tuning::TuningInstanceSingleCrit].
#'
#' @param object ([mlr3tuning::TuningInstanceSingleCrit].
#' @param type (`character(1)`):
#'   Type of the plot. Available choices:
#'   * `"marginal"`: scatter plots of hyperparameter versus performance. The
#'     colour of the points shows the batch number.
#'   * `"performance"`: scatter plots of batch number versus performance.
#'   * `"parameter"`: scatter plots of batch number versus hyperparameter. The
#'     colour of the points shows the performance.
#'   * `"parallel"` parallel coordinates plot. Parameter values are rescaled by
#'     `(x - mean(x)) / sd(x)`.
#'   * `"points"` - scatter plot of two hyperparameters versus performance. The
#'     colour of the points shows the performance.
#'   * `"surface"`: surface plot of 2 hyperparameters versus performance.
#'     The performance values are interpolated with the supplied
#'     [mlr3::Learner].
#' @param cols_x (`character()`)\cr
#'   Column names of hyperparameters. By default, all untransformed
#'   hyperparameters are plottet. Transformed hyperparameters are prefixed with
#'   `x_domain_`.
#' @param trafo (`logical(1)`)\cr
#'   Determines if untransformed (`FALSE`) or transformed (`TRUE`)
#'   hyperparametery are plotted.
#' @param learner ([mlr3::Learner])\cr
#'   Regression learner used to interpolate the data of the surface plot.
#' @param grid_resolution (`numeric()`)\cr
#'   Resolution of the surface plot.
#' @param ... (`any`):
#'   Additional arguments, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' if(requireNamespace("mlr3tuning") && requireNamespace("patchwork")) {
#' library(mlr3tuning)
#'
#' learner = lrn("classif.rpart")
#' learner$param_set$values$cp = to_tune(0.001, 0.1)
#' learner$param_set$values$minsplit = to_tune(1, 10)
#'
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 10))
#'
#' tuner = tnr("random_search")
#'
#' tuner$optimize(instance)
#'
#' # plot performance versus batch number
#' autoplot(instance, type = "performance")
#'
#' # plot cp values versus performance
#' autoplot(instance, type = "marginal", cols_x = "cp")
#'
#' # plot transformed parameter values versus batch number
#' autoplot(instance, type = "parameter", trafo = TRUE)
#'
#' # plot parallel coordinates plot
#' autoplot(instance, type = "parallel")}
autoplot.TuningInstanceSingleCrit = function(object, type = "marginal", cols_x = NULL, trafo = FALSE,
  learner = mlr3::lrn("regr.ranger"), grid_resolution = 100, ...) { # nolint
  assert_subset(cols_x, c(object$archive$cols_x, paste0("x_domain_", object$archive$cols_x)))
  assert_flag(trafo)

  if (is.null(cols_x)) {
    cols_x = if (trafo) {
      paste0("x_domain_", object$archive$cols_x)
    } else {
      object$archive$cols_x
    }
  }
  cols_y = object$archive$cols_y
  data = fortify(object)

  switch(type,
    "marginal" = {
      # each parameter versus performance
      plots  = map(cols_x, function(x) {
        data_i = data[!is.na(get(x)), c(x, cols_y, "batch_nr") , with = FALSE]
        ggplot(data_i, mapping = aes(x = .data[[x]], y = .data[[cols_y]])) +
          geom_point(aes(fill = .data$batch_nr), shape = 21, size = 3, stroke = 1) +
          scale_fill_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF"))
      })

      return(delayed_patchwork(plots, guides = "collect"))
    },

    "performance" = {
      # performance versus iteration
      max_to_min = if ("minimize" %in% object$archive$codomain$tags) min else max
      data[, "best" := max_to_min(get(cols_y)) == get(cols_y), by = "batch_nr"]
      ggplot(data, mapping = aes(x = .data$batch_nr, y = .data[[cols_y]])) +
        geom_point(mapping = aes(fill = .data$best), shape = 21, size = 3) +
        scale_fill_manual(name = "", labels = c(cols_y, "Best"), values = c("#FDE725FF", "#440154FF")) +
        geom_line(data = data[data$best, ], colour = "#440154FF", size = 1)
    },

    "parameter" = {
      # each parameter versus iteration
      plots  = map(cols_x, function(x) {
        ggplot(data, mapping = aes(x = .data$batch_nr, y = .data[[x]])) +
          geom_point(aes(fill = .data[[cols_y]]), shape = 21, size = 3, stroke = 0.5) +
          scale_fill_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF"))
      })

      return(delayed_patchwork(plots, guides = "collect"))
    },

    "parallel" = {
      # parallel coordinates plot
      data = data[, c(cols_x, cols_y), with = FALSE]
      if (any(data[, sapply(.SD, function(x) any(is.na(x)))])) {
        stop("Parallel coordinate plots cannot be displayed with missing data.")
      }
      x_axis = data.table(x = seq(names(data)), variable = names(data))

      # split data
      data_l = data[, .SD, .SDcols = which(sapply(data, function(x) is.character(x) || is.logical(x)))]
      data_n = data[, .SD, .SDcols = which(sapply(data, is.numeric))]
      data_y = data[, cols_y, with = FALSE]

      # factor columns to numeric
      data_c = data_l[, lapply(.SD, function(x) as.numeric(as.factor(x)))]

      # rescale
      data_n = data_n[, lapply(.SD, function(x) if (sd(x) > 0) (x - mean(x)) / sd(x) else rep(0, length(x)))]
      data_c = data_c[, lapply(.SD, function(x) if (sd(x) > 0) (x - mean(unique(x))) / sd(unique(x)) else rep(0, length(x)))]

      # to long format
      set(data_n, j = "id", value = seq_row(data_n))
      set(data_y, j = "id", value = seq_row(data_y))
      data_n = melt(data_n, measure.var = setdiff(names(data_n), "id"))

      if (nrow(data_c) > 0L) {
        # Skip if no factor column is present
        set(data_c, j = "id", value = seq_row(data_c))
        data_c = melt(data_c, measure.var = setdiff(names(data_c), "id"))
        data_l = data_l[, lapply(.SD, as.character)] # Logical to character
        data_l = melt(data_l, measure.var = names(data_l), value.name = "label")[, "label"]
        set(data_c, j = "label", value = data_l)
      }

      # merge
      data = rbindlist(list(data_c, data_n), fill = TRUE)
      data = merge(data, x_axis, by = "variable")
      data = merge(data, data_y, by = "id")
      setorderv(data, "x")

      ggplot(data, aes(x = .data$x, y = .data$value)) +
        geom_line(aes(group = .data$id, colour = .data[[cols_y]]), size = 1) +
        scale_colour_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF")) +
        geom_vline(aes(xintercept = x)) +
          {if (nrow(data_c) > 0L) geom_label(aes(label = .data$label), data[!is.na(data$label), ])} +
          scale_x_continuous(breaks = x_axis$x, labels = x_axis$variable) +
          theme(axis.title.x = element_blank())
    },

    "points" = {
      if (length(cols_x) != 2) {
        stop("Scatter plots can only be drawn with 2 parameters.")
      }

      ggplot(data, aes(x = .data[[cols_x[1]]], y = .data[[cols_x[2]]])) +
        geom_point(aes(fill = .data[[cols_y]]), data = data, shape = 21, size = 3, stroke = 1) +
        scale_fill_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF"))
    },

    "surface" = {
      if (length(cols_x) != 2) {
        stop("Surface plots can only be drawn with 2 parameters.")
      }

      data = data[, c(cols_x, cols_y), with = FALSE]
      task = mlr3::TaskRegr$new("surface", data, target = cols_y)

      mlr3::assert_learner(learner, task)
      assert_numeric(grid_resolution)

      learner$train(task)

      x_min = min(data[, cols_x[1], with = FALSE])
      x_max = max(data[, cols_x[1], with = FALSE])
      y_min = min(data[, cols_x[2], with = FALSE])
      y_max = max(data[, cols_x[2], with = FALSE])

      x = seq(from = x_min, to = x_max, by = (x_max - x_min) / grid_resolution)
      y = seq(from = y_min, to = y_max, by = (y_max - y_min) / grid_resolution)
      data_i = set_names(expand.grid(x, y), cols_x)

      setDT(data_i)[, (cols_y) := learner$predict_newdata(data_i)$response]

      ggplot(data_i, aes(x = .data[[cols_x[1]]], y = .data[[cols_x[2]]])) +
        geom_raster(aes(fill = .data[[cols_y]])) +
        scale_fill_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF")) +
        geom_point(aes(fill = .data[[cols_y]]), data = data, shape = 21, size = 3, stroke = 1)
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
fortify.TuningInstanceSingleCrit = function(model, data = NULL, ...) { # nolint
  as.data.table(model$archive)
}
