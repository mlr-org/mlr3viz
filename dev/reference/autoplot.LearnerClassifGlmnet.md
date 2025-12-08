# Plots for GLMNet Learners

Visualizations for
[mlr3learners::LearnerClassifGlmnet](https://mlr3learners.mlr-org.com/reference/mlr_learners_classif.glmnet.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"prediction"` (default): Decision boundary of the learner and the
  true class labels.

- `"ggfortify"`: Visualizes the model using the package
  [ggfortify](https://CRAN.R-project.org/package=ggfortify).

## Usage

``` r
# S3 method for class 'LearnerClassifCVGlmnet'
autoplot(
  object,
  type = "prediction",
  task = NULL,
  grid_points = 100L,
  expand_range = 0,
  theme = theme_minimal(),
  ...
)

# S3 method for class 'LearnerClassifGlmnet'
autoplot(
  object,
  type = "prediction",
  task = NULL,
  grid_points = 100L,
  expand_range = 0,
  theme = theme_minimal(),
  ...
)

# S3 method for class 'LearnerRegrCVGlmnet'
autoplot(
  object,
  type = "prediction",
  task = NULL,
  grid_points = 100L,
  expand_range = 0,
  theme = theme_minimal(),
  ...
)

# S3 method for class 'LearnerRegrGlmnet'
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

  ([mlr3learners::LearnerClassifGlmnet](https://mlr3learners.mlr-org.com/reference/mlr_learners_classif.glmnet.html)
  \|
  [mlr3learners::LearnerRegrGlmnet](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.glmnet.html)
  \|
  [mlr3learners::LearnerRegrCVGlmnet](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.cv_glmnet.html)
  \|
  [mlr3learners::LearnerRegrCVGlmnet](https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.cv_glmnet.html)).

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

## References

Tang Y, Horikoshi M, Li W (2016). “ggfortify: Unified Interface to
Visualize Statistical Result of Popular R Packages.” *The R Journal*,
**8**(2), 474–485.
[doi:10.32614/RJ-2016-060](https://doi.org/10.32614/RJ-2016-060) .

## Examples

``` r
if (FALSE) { # \dontrun{
library(mlr3)
library(mlr3viz)
library(mlr3learners)

# classification
task = tsk("sonar")
learner = lrn("classif.glmnet")
learner$train(task)
autoplot(learner, type = "ggfortify")

# regression
task = tsk("mtcars")
learner = lrn("regr.glmnet")
learner$train(task)
autoplot(learner, type = "ggfortify")
} # }
```
