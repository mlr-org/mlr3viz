# Plots for Regression Tasks

Visualizations for
[mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html). The
argument `type` controls what kind of plot is drawn. Possible choices
are:

- `"target"` (default): Box plot of the target variable.

- `"pairs"`: Passes data to
  [`GGally::ggpairs()`](https://ggobi.github.io/ggally/reference/ggpairs.html).
  Color is set to target column.

## Usage

``` r
# S3 method for class 'TaskRegr'
autoplot(object, type = "target", theme = theme_minimal(), ...)
```

## Arguments

- object:

  ([mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html)).

- type:

  (character(1)):  
  Type of the plot. See description.

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
if (mlr3misc::require_namespaces("GGally", quietly = TRUE)) {
task = tsk("mtcars")
task$select(c("am", "carb"))

head(fortify(task))
autoplot(task)
autoplot(task, type = "pairs")
}
```
