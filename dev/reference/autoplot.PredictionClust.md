# Plots for Cluster Predictions

Visualizations for
[mlr3cluster::PredictionClust](https://mlr3cluster.mlr-org.com/reference/PredictionClust.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"scatter"` (default): scatterplot with correlation values and colored
  cluster assignments.

- `"sil"`: Silhouette plot with mean silhouette value as the reference
  line. Requires package
  [ggfortify](https://CRAN.R-project.org/package=ggfortify).

- `"pca"`: Perform PCA on data and color code cluster assignments.
  Inspired by and uses
  [ggfortify::autoplot.kmeans](https://rdrr.io/pkg/ggfortify/man/autoplot.kmeans.html).

## Usage

``` r
# S3 method for class 'PredictionClust'
autoplot(
  object,
  task,
  row_ids = NULL,
  type = "scatter",
  theme = theme_minimal(),
  ...
)
```

## Arguments

- object:

  ([mlr3cluster::PredictionClust](https://mlr3cluster.mlr-org.com/reference/PredictionClust.html)).

- task:

  ([mlr3cluster::TaskClust](https://mlr3cluster.mlr-org.com/reference/TaskClust.html)).

- row_ids:

  ([`integer()`](https://rdrr.io/r/base/integer.html)) Row ids to subset
  task data to ensure that only the data used to make predictions are
  shown in plots.

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

## References

Tang Y, Horikoshi M, Li W (2016). “ggfortify: Unified Interface to
Visualize Statistical Result of Popular R Packages.” *The R Journal*,
**8**(2), 474–485.
[doi:10.32614/RJ-2016-060](https://doi.org/10.32614/RJ-2016-060) .

## Examples

``` r
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3cluster)
  library(mlr3viz)

  task = tsk("usarrests")
  learner = lrn("clust.kmeans", centers = 3)
  object = learner$train(task)$predict(task)

  head(fortify(object))
  autoplot(object, task)
}
#> Warning: Factor variables are omitted in plot
```
