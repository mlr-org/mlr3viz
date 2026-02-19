# Plots for Classification Predictions

Visualizations for
[mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html).
The argument `type` controls what kind of plot is drawn. Possible
choices are:

- `"stacked"` (default): Stacked barplot of true and estimated class
  labels.

- `"roc"`: ROC curve (1 - specificity on x, sensitivity on y). Requires
  package [precrec](https://CRAN.R-project.org/package=precrec).

- `"prc"`: Precision recall curve. Requires package
  [precrec](https://CRAN.R-project.org/package=precrec).

- `"threshold"`: Systematically varies the threshold of the
  [mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
  object and plots the resulting performance as returned by `measure`.

## Usage

``` r
# S3 method for class 'PredictionClassif'
autoplot(
  object,
  type = "stacked",
  measure = NULL,
  theme = theme_minimal(),
  ...
)
```

## Arguments

- object:

  ([mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)).

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

  (ignored).

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
if (mlr3misc::require_namespaces("precrec", quietly = TRUE)) {
task = tsk("spam")
learner = lrn("classif.rpart", predict_type = "prob")
object = learner$train(task)$predict(task)

head(fortify(object))
autoplot(object)
autoplot(object, type = "roc")
autoplot(object, type = "prc")
}

# }
```
