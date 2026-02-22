# Plot for Classification Learners

Visualizations for
[mlr3::LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"prediction"` (default): Decision boundary of the learner and the
  true class labels.

## Usage

``` r
# S3 method for class 'LearnerClassif'
autoplot(
  object,
  type = "prediction",
  task,
  grid_points = 100L,
  expand_range = 0,
  theme = theme_minimal(),
  ...
)
```

## Arguments

- object:

  ([mlr3::LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.html)).

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
# \donttest{
task = tsk("pima")$select(c("age", "pedigree"))
learner = lrn("classif.rpart", predict_type = "prob")
learner$train(task)

autoplot(learner, type = "prediction", task)

# }
```
