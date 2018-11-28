#' @title Plot for BenchmarkResult
#'
#' @description
#' Generates plots for [mlr3::BenchmarkResult].
#'
#' @param object \[[mlr3::BenchmarkResult]\].
#' @param type \[character(1)\]: Type of the plot.
#' @param by \[character(1)\]: Group plots by this variable.
#' @param ... \[any\]: Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' tasks = mlr_tasks$mget(c("iris", "pima", "sonar"))
#' learner = mlr_learners$mget(c("classif.featureless", "classif.rpart"))
#' resampling = mlr_resamplings$mget("cv")
#' object = benchmark(tasks, learner, resampling)
#'
#' library(ggplot2)
#' fortify(object)
#' autoplot(object)
autoplot.BenchmarkResult = function(object, type = "boxplot", by = "task_id", ...) {
  assert_choice(by, c("learner_id", "task_id"))
  x = setdiff(c("learner_id", "task_id"), by)

  switch(type,
    "boxplot" =
      ggplot(object, aes_string(x, "performance")) + geom_boxplot() + facet_wrap(by),
    stop("Unknown type")
  )
}

#' @export
fortify.BenchmarkResult = function(model, data, ...) {
  measure_ids = model$measures$measure_id
  data = as.data.table(model)[, c("hash", "task_id", "learner_id", "resampling_id", measure_ids), with = FALSE]
  melt(data, measure.vars = measure_ids,
    variable.name = "measure", value.name = "performance")
}
