# Plots for Rpart Learners

Visualizations for
[mlr3::LearnerClassifRpart](https://mlr3.mlr-org.com/reference/mlr_learners_classif.rpart.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"prediction"` (default): Decision boundary of the learner and the
  true class labels.

- `"ggparty"`: Visualizes the tree using the package
  [ggparty](https://CRAN.R-project.org/package=ggparty).

## Usage

``` r
# S3 method for class 'LearnerClassifRpart'
autoplot(
  object,
  type = "prediction",
  task = NULL,
  grid_points = 100L,
  expand_range = 0,
  theme = theme_minimal(),
  ...
)

# S3 method for class 'LearnerRegrRpart'
autoplot(
  object,
  type = "prediction",
  task = NULL,
  grid_points = 100L,
  expand_range = 0,
  theme = theme_minimal(),
  ...
)
```

## Arguments

- object:

  ([mlr3::LearnerClassifRpart](https://mlr3.mlr-org.com/reference/mlr_learners_classif.rpart.html)
  \|
  [mlr3::LearnerRegrRpart](https://mlr3.mlr-org.com/reference/mlr_learners_regr.rpart.html)).

- type:

  (character(1)):  
  Type of the plot. See description.

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Train task.

- grid_points:

  (integer(1))  
  Number of grid points per feature dimension.

- expand_range:

  (numeric(1))  
  Expand the range of the grid.

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
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3viz)

  # classification
  task = tsk("iris")
  learner = lrn("classif.rpart", keep_model = TRUE)
  learner$train(task)
  autoplot(learner, type = "ggparty")

  # regression
  task = tsk("mtcars")
  learner = lrn("regr.rpart", keep_model = TRUE)
  learner$train(task)
  autoplot(learner, type = "ggparty")
}
#> Warning: Ignoring unknown parameters: `label.size`
#> Warning: Ignoring unknown parameters: `label.size`
```
