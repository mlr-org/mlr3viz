# Plots for Benchmark Results

Visualizations for
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"boxplot"` (default): Boxplots of performance measures, one box per
  [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) and
  one facet per
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html).

- `"roc"`: ROC curve (1 - specificity on x, sensitivity on y). The
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
  may only have a single
  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) and a
  single
  [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html).
  Note that you can subset any
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
  with its `$filter()` method (see examples). Requires package
  [precrec](https://CRAN.R-project.org/package=precrec).

- `"prc"`: Precision recall curve. See `"roc"`.

- `"ci"`: Plot confidence intervals. Pass a `msr("ci", ...)` from the
  `mlr3inferr` package as argument `measure`.

## Usage

``` r
# S3 method for class 'BenchmarkResult'
autoplot(
  object,
  type = "boxplot",
  measure = NULL,
  theme = theme_minimal(),
  ...
)
```

## Arguments

- object:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)).

- type:

  (character(1)):  
  Type of the plot. See description.

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Performance measure to use.

- theme:

  ([`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html))  
  The
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  is applied by default to all plots.

- ...:

  arguments passed on to
  [`precrec::autoplot()`](https://rdrr.io/pkg/precrec/man/autoplot.html)
  for `type = "roc"` or `"prc"`. Useful to e.g. remove confidence bands
  with `show_cb = FALSE`.

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## References

Saito T, Rehmsmeier M (2017). “Precrec: fast and accurate
precision-recall and ROC curve calculations in R.” *Bioinformatics*,
**33**(1), 145-147.
[doi:10.1093/bioinformatics/btw570](https://doi.org/10.1093/bioinformatics/btw570)
.

## Examples

``` r
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3viz)

  tasks = tsks(c("pima", "sonar"))
  learner = lrns(c("classif.featureless", "classif.rpart"),
    predict_type = "prob")
  resampling = rsmps("cv")
  object = benchmark(benchmark_grid(tasks, learner, resampling))

  head(fortify(object))
  autoplot(object)
  autoplot(object$clone(deep = TRUE)$filter(task_ids = "pima"), type = "roc")
}
```
