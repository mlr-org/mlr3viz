#' @title Plot for TuningInstanceSingleCrit
#'
#' @description
#' Generates plots for [mlr3tuning::TuningInstanceSingleCrit].
#'
#' @param object ([mlr3tuning::TuningInstanceSingleCrit].
#' @param type (`character(1)`):
#'   Type of the plot. Available choices:
#'   * `"marginal"`: scatter plot of parameter versus performance. Points are 
#'     filled with batch number.
#'   * `"performance"`: scatter plot of batch number versus performance.
#'   * `"parameter"`: scatter plot of batch number versus parameter. Points are 
#'     filled with performance.
#' @param cols_x (`character()`)\cr
#'   Column names of hyperparameters. By default, all untransformed 
#'   hyperparameters are plottet. Transformed hyperparameters are prefixed with 
#'   `x_domain_`.
#' @param trafo (`logical(1)`)\cr
#'  Determines if untransformed (`FALSE`) or transformed (`TRUE`) 
#'  hyperparametery are plotted.
#' @param ... (`any`):
#'   Additional arguments, possibly passed down to the underlying plot functions.
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
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
autoplot.TuningInstanceSingleCrit = function(object, type = "marginal", cols_x = NULL, trafo = FALSE, ...) {
  assert_subset(cols_x, c(object$archive$cols_x, paste0("x_domain_", object$archive$cols_x)))
  assert_flag(trafo)

  if (is.null(cols_x)) {
    cols_x = if(trafo) {
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
      require_namespaces("patchwork")
      plots  = map(cols_x, function(x) {
        ggplot(data, mapping = aes(x = .data[[x]], y = .data[[cols_y]])) +
          geom_point(aes(fill = .data$batch_nr, stroke = 0.5), shape = 21, size = 3) +
          scale_fill_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF"))
      })  
      patchwork::wrap_plots(plots, guides = "collect")
    },

    "performance" = {
      # performance versus iteration
      max_to_min = if (has_element(object$archive$codomain$tags, "minimize")) min else max
      data[, "best":= max_to_min(get(cols_y)) == get(cols_y), by = "batch_nr"]
      ggplot(data, mapping = aes(x = .data$batch_nr, y = .data[[cols_y]])) +
        geom_point(mapping = aes(fill = .data$best), shape = 21, size = 3) +
        scale_fill_manual(name = "", labels = c(cols_y, "Best"), values = c("#FDE725FF", "#440154FF")) +
        geom_line(data = data[data$best,], colour = "#440154FF", size = 1)
    },

    "parameter" = {
      # each parameter versus iteration
      require_namespaces("patchwork")
      plots  = map(cols_x, function(x) {
        ggplot(data, mapping = aes(x = .data$batch_nr, y = .data[[x]])) +
          geom_point(aes(fill = .data[[cols_y]], stroke = 0.5), shape = 21, size = 3) +
          scale_fill_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF"))
      })  
      patchwork::wrap_plots(plots, guides = "collect")
    },

    "parallel" = {
      # parallel coordinates plot
      data = data[, c(cols_x, cols_y), with = FALSE]

      x_axis = data.table(x = seq(names(data)), variable = names(data))

      # split data
      data_l = data[, .SD, .SDcols = which(sapply(data, is.character))]
      data_n = data[, .SD, .SDcols = which(sapply(data, is.numeric))]
      data_y = data[, cols_y, with = FALSE]

      # factor columns to numeric
      data_c = data_l[, lapply(.SD, function(x) as.numeric(as.factor(x)))]

      # rescale
      data_n = data_n[, lapply(.SD, function(x) (x - mean(x)) / sd(x))]
      data_c = data_c[, lapply(.SD, function(x) (x - mean(unique(x))) / sd(unique(x)))]

      # configuration id
      data_c[, id := 1:nrow(data_c)]
      data_n[, id := 1:nrow(data_n)]
      data_y[, id := 1:nrow(data_y)]

      # to long format
      data_c = melt(data_c, measure.var = setdiff(names(data_c), "id"))
      data_l = melt(data_l, measure.var = names(data_l), value.name = "label")[, label]
      data_c[, label := data_l]
      data_n = melt(data_n, measure.var = setdiff(names(data_n), "id"))

      # merge
      data = rbindlist(list(data_c, data_n), fill = TRUE)
      data = merge(data, x_axis, by = "variable")
      data = merge(data, data_y, by = "id")
      setorderv(data, "x")

      ggplot(data, aes(x = .data$x, y = .data$value)) +
        geom_line(aes(group = id, colour = .data[[cols_y]]), size = 1) +
        scale_colour_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF")) +
        geom_vline(aes(xintercept = x)) +
        geom_label(aes(label = .data$label)) +
        scale_x_continuous(breaks = x_axis$x, labels = x_axis$variable) +
        theme(axis.title.x=element_blank())
    },

    stopf("Unknown plot type '%s'", type)
  )
}

fortify.TuningInstanceSingleCrit = function(model, data = NULL, ...) {
  as.data.table(model$archive)
}
