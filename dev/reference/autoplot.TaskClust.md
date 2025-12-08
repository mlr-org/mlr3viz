# Plots for Clustering Tasks

Visualizations for
[mlr3cluster::TaskClust](https://mlr3cluster.mlr-org.com/reference/TaskClust.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"pairs"` (default): Passes data
  [`GGally::ggpairs()`](https://ggobi.github.io/ggally/reference/ggpairs.html).

## Usage

``` r
# S3 method for class 'TaskClust'
autoplot(object, type = "pairs", theme = theme_minimal(), ...)
```

## Arguments

- object:

  ([mlr3cluster::TaskClust](https://mlr3cluster.mlr-org.com/reference/TaskClust.html)).

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
  library(mlr3cluster)
  library(mlr3viz)

  task = mlr_tasks$get("usarrests")

  head(fortify(task))
  autoplot(task)
}
```
