# Plots for Ensemble Feature Selection Results

Visualizations for
[EnsembleFSResult](https://mlr3fselect.mlr-org.com/reference/ensemble_fs_result.html).
The argument `type` determines the type of plot generated. The available
options are:

- `"pareto"` (default): Scatterplot of performance versus the number of
  features, possibly including the **Pareto front**, which allows users
  to decide how much performance they are willing to trade off for a
  more sparse model.

- `"performance"`: Boxplot of performance across the different learners
  used in the ensemble feature selection process. Each box represents
  the distribution of scores across different resampling iterations for
  a particular learner.

- `"n_features`: Boxplot of the number of features selected by each
  learner in the different resampling iterations.

- `"stability"`: Barplot of stability score for each learner used in the
  ensemble feature selection. This plot shows how similar are the output
  feature sets from each learner across the different resamplings.

## Usage

``` r
# S3 method for class 'EnsembleFSResult'
autoplot(
  object,
  type = "pareto",
  pareto_front = "stepwise",
  stability_measure = "jaccard",
  stability_args = NULL,
  theme = theme_minimal(),
  ...
)
```

## Arguments

- object:

  ([mlr3fselect::EnsembleFSResult](https://mlr3fselect.mlr-org.com/reference/ensemble_fs_result.html)).

- type:

  (character(1)):  
  Type of the plot. See description.

- pareto_front:

  (`character(1)`)  
  Type of pareto front to plot. Can be `"stepwise"` (default),
  `"estimated"` or `"none"`.

- stability_measure:

  (`character(1)`)  
  The stability measure to be used in case `type = "stability"`. One of
  the measures returned by
  [`stabm::listStabilityMeasures()`](https://bommert.github.io/stabm/reference/listStabilityMeasures.html)
  in lower case. Default is `"jaccard"`.

- stability_args:

  (`list`)  
  Additional arguments passed to the stability measure function.

- theme:

  ([`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html))  
  The
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  is applied by default to all plots.

- ...:

  (ignored).

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
# \donttest{
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3fselect)

  set.seed (42)
  efsr = ensemble_fselect(
    fselector = fs("random_search"),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 5),
    inner_resampling = rsmp("cv", folds = 3),
    inner_measure = msr("classif.ce"),
    measure = msr("classif.acc"),
    terminator = trm("evals", n_evals = 5)
  )

  # Pareto front (default, stepwise)
  autoplot(efsr)

  # Pareto front (estimated)
  autoplot(efsr, pareto_front = "estimated")

  # Performance
  autoplot(efsr, type = "performance")

  # Number of features
  autoplot(efsr, type = "n_features")

  # stability
  autoplot(efsr, type = "stability")

  # use inner measure
  efsr$set_active_measure("inner")

  # Pareto front uses now the classification error
  autoplot(efsr)
}

# }
```
