# Plots for Resample Results

Visualizations for
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"boxplot"` (default): Boxplot of performance measures.

- `"histogram"`: Histogram of performance measures.

- `"roc"`: ROC curve (1 - specificity on x, sensitivity on y). The
  predictions of the individual
  [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)s
  are merged prior to calculating the ROC curve (micro averaged).
  Requires package
  [precrec](https://CRAN.R-project.org/package=precrec).

- `"prc"`: Precision recall curve. See `"roc"`.

- `"prediction"`: Plots the learner prediction for a grid of points.
  Needs models to be stored. Set `store_models = TRUE` for
  [`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html).
  For classification, we support tasks with exactly two features and
  learners with `predict_type=` set to `"response"` or `"prob"`. For
  regression, we support tasks with one or two features. For tasks with
  one feature we can print confidence bounds if the predict type of the
  learner was set to `"se"`. For tasks with two features the predict
  type will be ignored.

## Usage

``` r
# S3 method for class 'ResampleResult'
autoplot(
  object,
  type = "boxplot",
  measure = NULL,
  predict_sets = "test",
  binwidth = NULL,
  theme = theme_minimal(),
  ...
)
```

## Arguments

- object:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)).

- type:

  (character(1)):  
  Type of the plot. See description.

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Performance measure to use.

- predict_sets:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Only for `type` set to `"prediction"`. Which points should be shown in
  the plot? Can be a subset of (`"train"`, `"test"`) or empty.

- binwidth:

  (`integer(1)`)  
  Width of the bins for the histogram.

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
# \donttest{
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3viz)

  task = tsk("sonar")
  learner = lrn("classif.rpart", predict_type = "prob")
  resampling = rsmp("cv", folds = 3)
  object = resample(task, learner, resampling)

  head(fortify(object))

  # Default: boxplot
  autoplot(object)

  # Histogram
  autoplot(object, type = "histogram", bins = 30)

  # ROC curve, averaged over resampling folds:
  autoplot(object, type = "roc")

  # ROC curve of joint prediction object:
  autoplot(object$prediction(), type = "roc")

  # Precision Recall Curve
  autoplot(object, type = "prc")

  # Prediction Plot
  task = tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))
  resampling = rsmp("cv", folds = 3)
  object = resample(task, learner, resampling, store_models = TRUE)
  autoplot(object, type = "prediction")
}

# }
```
