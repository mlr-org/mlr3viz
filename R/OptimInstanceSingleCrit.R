#' @title Plots for Optimization Instances
#'
#' @importFrom scales pretty_breaks
#'
#' @description
#' Visualizations for [bbotk::OptimInstanceSingleCrit].
#' The argument `type` controls what kind of plot is drawn.
#' Possible choices are:
#'
#'   * `"marginal"` (default): Scatter plots of x versus y.
#'      The color of the points shows the batch number.
#'   * `"performance"`: Scatter plots of batch number versus y
#'   * `"parameter"`: Scatter plots of batch number versus input.
#'      The color of the points shows the y values.
#'   * `"parallel"`: Parallel coordinates plot.
#'      x values are rescaled by `(x - mean(x)) / sd(x)`.
#'   * `"points"`: Scatter plot of two x dimensions versus.
#'      The color of the points shows the y values.
#'   * `"surface"`: Surface plot of two x dimensions versus y values.
#'     The y values are interpolated with the supplied [mlr3::Learner].
#'   * `"pairs"`: Plots all x and y values against each other.
#'   * `"incumbent"`: Plots the incumbent versus the number of configurations.
#'
#' @param object ([bbotk::OptimInstanceSingleCrit]).
#' @template param_type
#' @param cols_x (`character()`)\cr
#'  Column names of x values.
#'  By default, all untransformed x values from the search space are plotted.
#'  Transformed hyperparameters are prefixed with `x_domain_`.
#' @param trafo (`logical(1)`)\cr
#'  If `FALSE` (default), the untransformed x values are plotted.
#'  If `TRUE`, the transformed x values are plotted.
#' @param learner ([mlr3::Learner])\cr
#'   Regression learner used to interpolate the data of the surface plot.
#' @param grid_resolution (`numeric()`)\cr
#'   Resolution of the surface plot.
#' @param batch (`integer()`)\cr
#'  The batch number(s) to limit the plot to.
#'  The default is all batches.
#' @template param_theme
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#'
#' @export
#' @examples
#' if (requireNamespace("mlr3") && requireNamespace("bbotk") && requireNamespace("patchwork")) {
#'   library(bbotk)
#'   library(paradox)
#'
#'   fun = function(xs) {
#'     c(y = -(xs[[1]] - 2)^2 - (xs[[2]] + 3)^2 + 10)
#'   }
#'   domain = ps(
#'     x1 = p_dbl(-10, 10),
#'     x2 = p_dbl(-5, 5)
#'   )
#'   codomain = ps(
#'     y = p_dbl(tags = "maximize")
#'   )
#'   obfun = ObjectiveRFun$new(
#'     fun = fun,
#'     domain = domain,
#'     codomain = codomain
#'   )
#'
#'   instance = OptimInstanceSingleCrit$new(objective = obfun, terminator = trm("evals", n_evals = 20))
#'
#'   optimizer = opt("random_search", batch_size = 2)
#'   optimizer$optimize(instance)
#'
#'   # plot y versus batch number
#'   print(autoplot(instance, type = "performance"))
#'
#'   # plot x1 values versus performance
#'   print(autoplot(instance, type = "marginal", cols_x = "x1"))
#'
#'   # plot parallel coordinates plot
#'   print(autoplot(instance, type = "parallel"))
#'
#'   # plot pairs
#'   print(autoplot(instance, type = "pairs"))
#'
#'   # plot incumbent
#'   print(autoplot(instance, type = "incumbent"))
#' }
autoplot.OptimInstanceSingleCrit = function(object, type = "marginal", cols_x = NULL, trafo = FALSE, learner = mlr3::lrn("regr.ranger"), grid_resolution = 100, batch = NULL, theme = theme_minimal(), ...) { # nolint
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
  if (is.null(batch)) batch = seq_len(object$archive$n_batch)
  assert_subset(batch, seq_len(object$archive$n_batch))
  data = data[list(batch), , on = "batch_nr"]

  switch(type,
    "marginal" = {
      # each parameter versus performance
      plots = map(cols_x, function(x) {
        data_i = data[!is.na(get(x)), c(x, cols_y, "batch_nr"), with = FALSE]
        breaks = pretty(data_i$batch_nr, n = 4)
        breaks[1] = min(data_i$batch_nr)
        breaks[length(breaks)] = max(data_i$batch_nr)
        data_i[, "batch_nr" := as.factor(get("batch_nr"))]

        ggplot(data_i,
          mapping = aes(x = .data[[x]],
          y = .data[[cols_y]])
          ) +
          geom_point(
            mapping = aes(fill = .data$batch_nr),
            shape = 21,
            size = 3,
            stroke = 0.5,
            alpha = 0.8) +
          scale_fill_viridis_d("Batch", breaks = breaks) +
          theme
      })

      return(delayed_patchwork(plots, guides = "collect"))
    },

    "performance" = {
      # performance versus iteration
      max_to_min = if ("minimize" %in% object$archive$codomain$tags) which.min else which.max

      data[, "group" := factor(1, labels = "Objective value")]
      top_batch = data[, .SD[max_to_min(get(cols_y))], by = "batch_nr"]
      top_batch[, "group" := factor(1, labels = "Best value")]

      ggplot() +
        geom_line(top_batch,
          mapping = aes(
            x = .data[["batch_nr"]],
            y = .data[[cols_y]],
            color = .data[["group"]]),
          group = 1,
          linewidth = 1) +
        geom_point(data,
          mapping = aes(
            x = .data[["batch_nr"]],
            y = .data[[cols_y]],
            fill = .data[["group"]]),
          shape = 21,
          size = 3,
          stroke = 0.5,
          alpha = 0.8) +
        xlab("Batch") +
        scale_y_continuous(breaks = pretty_breaks()) +
        scale_fill_manual(values = viridis::viridis(1, begin = 0.33)) +
        scale_color_manual(values = viridis::viridis(1, begin = 0.5)) +
        theme +
        theme(legend.title = element_blank())
    },

    "parameter" = {
      # each parameter versus iteration
      plots = map(cols_x, function(x) {
        ggplot(data,
          mapping = aes(
            x = .data$batch_nr,
            y = .data[[x]])) +
          geom_point(
            mapping = aes(
              fill = .data[[cols_y]]),
              shape = 21,
              size = 3,
              stroke = 0.5,
              alpha = 0.8) +
          guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
          scale_fill_viridis_c(breaks = scales::pretty_breaks()) +
          theme
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

      if (nrow(data_c)) {
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

      ggplot(data,
        mapping = aes(
          x = .data[["x"]],
          y = .data[["value"]])) +
        geom_line(
          mapping = aes(
            group = .data$id,
            color = .data[[cols_y]]),
          linewidth = 1) +
        geom_vline(aes(xintercept = x)) +
        {
          if (nrow(data_c)) geom_label(
            mapping = aes(label = .data$label),
            data = data[!is.na(data$label), ])
        } +
        scale_x_continuous(breaks = x_axis$x, labels = x_axis$variable) +
        scale_color_viridis_c() +
        guides(color = guide_colorbar(barwidth = 0.5, barheight = 10)) +
        theme +
        theme(axis.title.x = element_blank())
    },

    "points" = {
      if (length(cols_x) != 2) {
        stop("Scatter plots can only be drawn with 2 parameters.")
      }

      ggplot(data,
        mapping = aes(
          x = .data[[cols_x[1]]],
          y = .data[[cols_x[2]]])) +
        geom_point(
          mapping = aes(fill = .data[[cols_y]]),
          data = data,
          shape = 21,
          size = 3,
          stroke = 1) +
        scale_fill_viridis_c() +
        guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
        theme
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

      ggplot(data_i,
        mapping = aes(
          x = .data[[cols_x[1]]],
          y = .data[[cols_x[2]]])) +
        geom_raster(
          mapping = aes(fill = .data[[cols_y]])) +
        geom_point(
          mapping = aes(fill = .data[[cols_y]]),
          data = data,
          shape = 21,
          size = 3,
          stroke = 0.5,
          alpha = 0.8) +
        scale_x_continuous(expand = c(0.01, 0.01)) +
        scale_y_continuous(expand = c(0.01, 0.01)) +
        guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
        scale_fill_viridis_c() +
        theme
    },

    "pairs" = {
      require_namespaces("GGally")

      color = viridis::viridis(1, begin = 0.5)
      alpha = 0.8

      GGally::ggpairs(data[, c(cols_x, cols_y, "batch_nr"), with = FALSE],
        switch = "both",
        upper = list(continuous = "cor",  combo = GGally::wrap("box_no_facet", fill = color, alpha = alpha), discrete = "count", na = "na"),
        lower = list(continuous = GGally::wrap("points", color = color), combo = GGally::wrap("facethist", fill = color, alpha = alpha), discrete = GGally::wrap("facetbar", fill = color, alpha = alpha), na = "na"),
        diag = list(continuous = GGally::wrap("densityDiag", color = color), discrete = GGally::wrap("barDiag", fill = color, alpha = alpha), na = "naDiag")) +
        theme
    },

    "incumbent" = {
      data[, "incumbent" := cummin(.SD[[1]]), .SDcols = cols_y]

      ggplot(data,
        mapping = aes(
          x = seq_row(data),
          y = .data[["incumbent"]],
          lty = cols_y)) +
        geom_step(
          linewidth = 1,
          color = viridis::viridis(1, begin = 0.5)) +
        xlab("Number of Configurations") +
        ylab(cols_y) +
        scale_linetype(name = "Incumbent") +
        theme
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
fortify.OptimInstanceSingleCrit = function(model, data = NULL, ...) { # nolint
  as.data.table(model$archive)
}
