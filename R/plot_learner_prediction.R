#' @title Plot for Learner Predictions
#'
#' @description
#' Generates plots for [mlr3::Learner], and [mlr3::Task].
#'
#' * For classification we support tasks with two features and learners with `predict_type="response"` and `"prob"`.
#' * For regression we support tasks with one or two features.
#'   For tasks with one feature we can print confidence bounds if the predict type of the learner was set to `"se"`.
#'   For tasks with two features the predict type will be ignored.
#'
#' @param learner ([mlr3::Learner])
#' @param task ([mlr3::Task])
#' @param grid_points (`integer(1)`)\cr
#'   Resolution of the grid.
#'   For factors, ordered and logicals this value is ignored.
#' @param expand_range (`numeric(1)`)\cr
#'   Expand the prediction range for numerical features.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3viz)
#'
#' task = tsk("pima")$select(c("age", "glucose"))
#' learner = lrn("classif.rpart", predict_type = "prob")
#' p = plot_learner_prediction(learner, task)
plot_learner_prediction = function(learner, task, grid_points = 100L, expand_range = 0) {
  UseMethod("plot_learner_prediction")
}

#' @export
plot_learner_prediction.LearnerClassif = function(learner, task, grid_points = 100L, expand_range = 0) {
  features = task$feature_names
  if (length(features) != 2) {
    stop("plot_learner_prediction.TaskClassif only works for tasks with two features!")
  }

  grid = predict_grid(learner, task, grid_points, expand_range)

  if (learner$predict_type == "prob") {
    grid[, "prob.response" := .SD[, paste0("prob.", get("response")), with = FALSE] , by = "response"]
    raster_aes = aes_string(fill = "response", alpha = "prob.response")
  } else {
    raster_aes = aes_string(fill = "response")
  }

  ggplot(grid, aes_string(features[1L], features[2L])) +
    geom_raster(raster_aes) +
    geom_point(data = task$data(), aes_string(features[1L], features[2L], fill = task$target_names), shape = 21, size = 4, color = "black")
}


#' @export
plot_learner_prediction.LearnerRegr = function(learner, task, grid_points = 100L, expand_range = 0) {
  features = task$feature_names
  if (length(features) > 2) {
    stop("plot_learner_prediction.LearnerRegr only works with one or two features!")
  }

  grid = predict_grid(learner, task, grid_points, expand_range)

  if (length(features) == 1) {
    if (learner$predict_type == "se") {
      se_geom = geom_ribbon(aes_string(ymin = "response-se", ymax = "response+se"), alpha = 0.2)
    } else {
      se_geom = NULL
    }
    g = ggplot(grid, aes_string(features, "response")) +
      se_geom +
      geom_line() +
      geom_point(data = task$data(), aes_string(features, task$target_names))
  } else if (length(features) == 2) {
    g = ggplot(grid, aes_string(features[1L], features[2L])) +
      geom_raster(aes_string(fill = "response")) +
      geom_point(data = task$data(), aes_string(features[1L], features[2L], fill = task$target_names), shape = 21, size = 4, color = "black")
  }
  return(g)

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

predict_grid = function(learner, task, grid_points, expand_range) {
  features = task$feature_names
  learner = learner$clone()$train(task)
  grid = map(task$data(cols = features), sequenize, n = grid_points, expand_range = expand_range)
  grid = cross_join(grid, sorted = FALSE)
  grid = cbind(grid, remove_named(as.data.table(learner$predict_newdata(grid)), c("row_id", "truth")))
  return(grid)
}


