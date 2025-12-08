# Plots for Learner Predictions

Visualizations for the
[mlr3::Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)
of a single
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) on a
single [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html).

- For classification we support tasks with exactly two features and
  learners with `predict_type` set to `"response"` or `"prob"`.

- For regression we support tasks with one or two features. For tasks
  with one feature we print confidence bounds if the predict type of the
  learner was set to `"se"`. For tasks with two features the predict
  type will be ignored.

Note that this function is a wrapper around
[`autoplot.ResampleResult()`](https://mlr3viz.mlr-org.com/dev/reference/autoplot.ResampleResult.md)
for a temporary
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
using
[mlr3::mlr_resamplings_holdout](https://mlr3.mlr-org.com/reference/mlr_resamplings_holdout.html)
with ratio 1 (all observations in the training set).

## Usage

``` r
plot_learner_prediction(learner, task, grid_points = 100L, expand_range = 0)
```

## Arguments

- learner:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)).

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- grid_points:

  (`integer(1)`)  
  Resolution of the grid. For factors, ordered and logicals this value
  is ignored.

- expand_range:

  (`numeric(1)`)  
  Expand the prediction range for numerical features.

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
# \donttest{
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3viz)

  task = mlr3::tsk("pima")$select(c("age", "glucose"))
  learner = lrn("classif.rpart", predict_type = "prob")
  p = plot_learner_prediction(learner, task)
  print(p)
}
#> Warning: Removed 5 rows containing missing values or values outside the scale range
#> (`geom_point()`).

# }
```
