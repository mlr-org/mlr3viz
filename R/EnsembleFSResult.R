#' @title Plots for Ensemble Feature Selection Results
#'
#' @description
#' Visualizations for [EnsembleFSResult][mlr3fselect::EnsembleFSResult].
#' The argument `type` determines the type of plot generated.
#' The available options are:
#'
#' * `"pareto"` (default): Scatterplot of performance versus the number of
#' features, possibly including the **Pareto front**, which allows users to
#' decide how much performance they are willing to trade off for a more sparse
#' model.
#' * `"performance"`: Boxplot of performance across the different learners
#' used in the ensemble feature selection process.
#' Each box represents the distribution of scores across different resampling
#' iterations for a particular learner.
#' * `"n_features`: Boxplot of the number of features selected by each learner
#' in the different resampling iterations.
#' * `"stability"`: Barplot of stability score for each learner used in the
#' ensemble feature selection. This plot shows how similar are the output feature
#' sets from each learner across the different resamplings.
#'
#' @param object ([mlr3fselect::EnsembleFSResult]).
#' @template param_type
#' @param pareto_front (`character(1)`)\cr
#'  Type of pareto front to plot. Can be `"stepwise"` (default), `"estimated"`
#'  or `"none"`.
#' @param stability_measure (`character(1)`)\cr
#'  The stability measure to be used in case `type = "stability"`.
#'  One of the measures returned by [stabm::listStabilityMeasures()] in lower case.
#'  Default is `"jaccard"`.
#' @template param_theme
#' @param stability_args (`list`)\cr
#'  Additional arguments passed to the stability measure function.
#' @param ... (ignored).
#'
#' @return [ggplot2::ggplot()].
#' @examples
#' \donttest{
#' if (requireNamespace("mlr3")) {
#'   library(mlr3)
#'   library(mlr3fselect)
#'
#'   set.seed (42)
#'   efsr = ensemble_fselect(
#'     fselector = fs("random_search"),
#'     task = tsk("sonar"),
#'     learners = lrns(c("classif.rpart", "classif.featureless")),
#'     init_resampling = rsmp("subsampling", repeats = 5),
#'     inner_resampling = rsmp("cv", folds = 3),
#'     measure = msr("classif.ce"),
#'     terminator = trm("evals", n_evals = 5)
#'   )
#'
#'   # Pareto front (default, stepwise)
#'   autoplot(efsr)
#'
#'   # Pareto front (estimated)
#'   autoplot(efsr, pareto_front = "estimated")
#'
#'   # Performance
#'   autoplot(efsr, type = "performance")
#'
#'   # Number of features
#'   autoplot(efsr, type = "n_features")
#'
#'   # stability
#'   autoplot(efsr, type = "stability")
#' }
#' }
#' @export
autoplot.EnsembleFSResult = function(
  object,
  type = "pareto",
  pareto_front = "stepwise",
  stability_measure = "jaccard",
  stability_args = NULL,
  theme = theme_minimal(),
  ...
  ) {
  assert_string(type)
  assert_choice(pareto_front, choices = c("stepwise", "estimated", "none"))
  result = object$result
  measure_id = object$measure

  switch(type,
    "pareto" = {
      p = ggplot(result, mapping = aes(
        x = .data[["n_features"]],
        y = .data[[measure_id]],
        color = .data[["learner_id"]])) +
        geom_point() +
        scale_color_viridis_d("Learner ID", end = 0.8, alpha = 0.8) +
        xlab("Number of Features") +
        ylab(measure_id) +
        theme

      if (pareto_front == "stepwise") {
        pf = object$pareto_front(type = "empirical")
        pf_step = stepwise_pf(pf)
        p = p +
          geom_line(data = pf_step, mapping = aes(
            x = .data[["n_features"]],
            y = .data[[measure_id]]),
            color = "black", linewidth = 0.7)
      } else if (pareto_front == "estimated") {
        pfe = object$pareto_front(type = "estimated")
        p = p +
          geom_line(data = pfe, mapping = aes(
            x = .data[["n_features"]],
            y = .data[[measure_id]]),
            color = "black", linetype = "dashed", linewidth = 0.7)
      }

      p
    },

    "performance" = {
      ggplot(result, aes(
        x = .data[["learner_id"]],
        y = .data[[measure_id]],
        fill = .data[["learner_id"]])) +
        geom_boxplot(show.legend = FALSE) +
        scale_fill_viridis_d(end = 0.8, alpha = 0.8) +
        ylab(measure_id) +
        theme +
        theme(axis.title.x = element_blank())
    },

    "n_features" = {
      ggplot(result, aes(
        x = .data[["learner_id"]],
        y = .data[["n_features"]],
        fill = .data[["learner_id"]]))+
        geom_boxplot(show.legend = FALSE) +
        scale_fill_viridis_d(end = 0.8, alpha = 0.8) +
        ylab("Number of Features") +
        theme +
        theme(axis.title.x = element_blank())
    },

    "stability" = {
      # get stability per learner
      stab_res = object$stability(
        stability_measure = stability_measure,
        stability_args = stability_args,
        global = FALSE,
        reset_cache = FALSE)
      data = data.table(learner_id = names(stab_res), value = stab_res)

      ggplot(data, mapping = aes(
        x = .data[["learner_id"]],
        y = .data[["value"]],
        fill = .data[["learner_id"]])) +
        geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
        scale_fill_viridis_d(end = 0.8, alpha = 0.8) +
        ylab(stability_measure) +
        theme +
        theme(axis.title.x = element_blank())
    },

    stopf("Unknown plot type '%s'", type)
  )
}

# Adds more points to the empirical Pareto front to create a more step-wise-looking front.
# "pf" is a data.table from (see "EnsembleFSResult$pareto_front()") with 1st column
# "n_features" and 2nd column the performance measure
stepwise_pf = function(pf) {
  measure_id = setdiff(names(pf), "n_features")

  # step-wise pareto front
  pf_step = data.table(n_features = numeric(0))
  pf_step[, (measure_id) := numeric(0)]

  for (i in seq_row(pf)) {
    # add the pareto point
    pf_step = rbind(pf_step, pf[i])

    # add intermediate point if applicable
    if (i < nrow(pf)) {
      ok = pf[["n_features"]][i+1] > pf[["n_features"]][i]

      if (ok) {
        # more features, previous performance score
        intermediate_point = data.table(n_features = pf[["n_features"]][i+1])
        intermediate_point[, (measure_id) := pf[[measure_id]][i]]
        pf_step = rbind(pf_step, intermediate_point)
      }
    }
  }

  pf_step
}
