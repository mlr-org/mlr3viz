# Plots for Filter Scores

Visualizations for
[mlr3filters::Filter](https://mlr3filters.mlr-org.com/reference/Filter.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"barplot"` (default): Bar plot of filter scores.

## Usage

``` r
# S3 method for class 'Filter'
autoplot(object, type = "boxplot", n = Inf, theme = theme_minimal(), ...)
```

## Arguments

- object:

  ([mlr3filters::Filter](https://mlr3filters.mlr-org.com/reference/Filter.html)).

- type:

  (character(1)):  
  Type of the plot. See description.

- n:

  (`integer(1)`)  
  Only include the first `n` features with the highest importance.
  Defaults to all features.

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
if (mlr3misc::require_namespaces("mlr3filters", quietly = TRUE)) {
library(mlr3filters)

task = tsk("mtcars")
f = flt("correlation")
f$calculate(task)

head(fortify(f))
autoplot(f, n = 5)
}
```
