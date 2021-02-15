#' @title Plot for TuningInstanceSingleCrit
#'
#' @description
#' Generates plots for [mlr3tuning::TuningInstanceSingleCrit].
#'
#' @param object ([mlr3tuning::TuningInstanceSingleCrit].
#' @param type (`character(1)`):
#'   Type of the plot. Available choices:
#'   * `"marginal"`: scatter plot of parameter versus performance. Points are filled with batch number.
#'   * `"performance"`: scatter plot of performance versus batch number.
#'   * `"parameter"`: scatter plot of parameter versus batch number. Points are filled with performance.
#'   * `"freqpoly"`: frequency polygon plot with `ggplot2::geom_freqpoly`.
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
      data[, best:= max_to_min(get(cols_y)) == get(cols_y), by = batch_nr]
      ggplot(data, mapping = aes(x = .data[[cols_y]], y = .data$batch_nr)) +
        geom_point(mapping = aes(fill = .data$best), shape = 21, size = 3) +
        scale_fill_manual(name = "", labels = c(cols_y, "Best"), values = c("#FDE725FF", "#440154FF")) +
        geom_line(, data = data[(best)], colour = "#440154FF", size = 1)
    },

    "parameter" = {
      # each parameter versus iteration
      require_namespaces("patchwork")
      plots  = map(cols_x, function(x) {
        ggplot(data, mapping = aes(x = .data[[x]], y = .data$batch_nr)) +
          geom_point(aes(fill = .data[[cols_y]], stroke = 0.5), shape = 21, size = 3) +
          scale_fill_gradientn(colours = c("#FDE725FF", "#21908CFF", "#440154FF"))
      })  
      patchwork::wrap_plots(plots, guides = "collect")
    }
  )

  stopf("Unknown plot type '%s'", type)
}

fortify.TuningInstanceSingleCrit = function(model, data = NULL, ...) {
  as.data.table(model$archive)
}