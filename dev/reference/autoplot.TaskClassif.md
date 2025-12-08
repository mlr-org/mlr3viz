# Plots for Classification Tasks

Visualizations for
[mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"target"` (default): Bar plot of the target variable (default).

- `"duo"`: Passes data to
  [`GGally::ggduo()`](https://ggobi.github.io/ggally/reference/ggduo.html).
  `columnsX` is the target and `columnsY` are the features.

- `"pairs"`: Passes data to
  [`GGally::ggpairs()`](https://ggobi.github.io/ggally/reference/ggpairs.html).
  Color is set to target column.

## Usage

``` r
# S3 method for class 'TaskClassif'
autoplot(object, type = "target", theme = theme_minimal(), ...)
```

## Arguments

- object:

  ([mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html)).

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
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3viz)

  task = tsk("iris")

  head(fortify(task))
  autoplot(task)
  autoplot(task$clone()$select(c("Sepal.Length", "Sepal.Width")),
    type = "pairs")
  autoplot(task, type = "duo")
}
```
