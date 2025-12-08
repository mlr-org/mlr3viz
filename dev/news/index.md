# Changelog

## mlr3viz (development version)

- fix: Suppress warnings about unused arguments in
  [`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  when plotting ROC/PRC curves with newer versions of ggplot2.
- refactor: plot for `LearnerSurvCoxPH` will be moved to
  `mlr3proba@0.8.4`

## mlr3viz 0.10.1

CRAN release: 2025-01-16

- feat: Allow passing parameters to
  [`precrec::autoplot()`](https://rdrr.io/pkg/precrec/man/autoplot.html)
  (eg `show_cb`) when plotting `BenchmarkResult` and `ResampleResult`
  objects, using `type` = `roc` or `prc`.
- refactor: Wrong `type` in `autoplot`s now gives hints of which ones to
  use.
- refactor: Update `EnsembleFSResult.autoplot` to use the
  `active_measure` field.
- feat: add plot for confidence intervals (`mlr3inferr`).

## mlr3viz 0.10.0

CRAN release: 2024-11-07

- Add plot for `LearnerSurvCoxPH`.

## mlr3viz 0.9.0

CRAN release: 2024-07-01

- Work with new bbotk 0.9.0 and mlr3tuning 0.21.0
- Add plots for `EnsembleFSResult` object.

## mlr3viz 0.8.0

CRAN release: 2024-03-05

- Work with new paradox version 1.0.0

## mlr3viz 0.7.0

CRAN release: 2023-12-21

- Add `"prediction"` plots for classification and regression learners.

## mlr3viz 0.6.2

CRAN release: 2023-11-23

- Fix snapshots for bbotk 0.7.3.
- Add `"incumbent"` plot for `OptimInstanceSingleCrit`.

## mlr3viz 0.6.1

CRAN release: 2023-01-23

- Add `binwidth` argument to histogram plots.
- The `"performance"` plot always connected the maximum performance
  values. Now the minimum values are connected when the measure is
  minimized.

## mlr3viz 0.6.0

CRAN release: 2023-01-12

- Add `theme` option to
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  functions to supply a
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
  The default is
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
- Remove `theme_mlr3()`.
- Unify plot layouts.
- Remove support for passing extra arguments to `geom_` functions via
  `...`. This behavior was not consistent across the
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  functions.

## mlr3viz 0.5.10

CRAN release: 2022-08-15

- Improved documentation.
- Make checks run without suggested packages.

## mlr3viz 0.5.9

CRAN release: 2022-05-25

- Plots for survival objects moved to mlr3proba.

## mlr3viz 0.5.8

CRAN release: 2022-04-04

- Compatibility fixes.

## mlr3viz 0.5.7

CRAN release: 2021-10-14

- Compatibility fix for testthat.

## mlr3viz 0.5.6

CRAN release: 2021-09-14

- Compatibility fix for mlr3tuning.
- Fixed position of labels in barplot for `PredictionClassif`.

## mlr3viz 0.5.5

CRAN release: 2021-08-12

- Fixed another bug for ROC- and Precision-recall-curves
  ([\#79](https://github.com/mlr-org/mlr3viz/issues/79)).

## mlr3viz 0.5.4

CRAN release: 2021-07-01

- Fixed a bug for ROC- and Precision-recall-curves
  ([\#72](https://github.com/mlr-org/mlr3viz/issues/72),
  [\#75](https://github.com/mlr-org/mlr3viz/issues/75)).

## mlr3viz 0.5.3

CRAN release: 2021-03-15

- New autoplot for `LearnerClustHclust`.

## mlr3viz 0.5.2

CRAN release: 2021-03-08

- New autoplot for `TuningInstanceSingleCrit` from package `mlr3tuning`.
- Fixed bugs in autoplot function for `BenchmarkResult`
  ([\#63](https://github.com/mlr-org/mlr3viz/issues/63),
  [\#65](https://github.com/mlr-org/mlr3viz/issues/65)).
- Fixed a bug in autoplot function for `PredictionClust`
  ([\#67](https://github.com/mlr-org/mlr3viz/issues/67)).

## mlr3viz 0.5.1

CRAN release: 2021-01-27

- Fix test on solaris.

## mlr3viz 0.5.0

CRAN release: 2020-11-02

- New autoplot for `PredictionSurv`.
- New autoplots for learners from package `glmnet` via `ggfortify`.
- Fixed ROC and PRC plots for resampling `"holdout"`
  ([\#54](https://github.com/mlr-org/mlr3viz/issues/54)).
- If possible, we now show confidence bounds for ROC and PRC plots
  ([\#55](https://github.com/mlr-org/mlr3viz/issues/55)).
- Fixed a bug in autoplot function `TaskDens`
  ([\#57](https://github.com/mlr-org/mlr3viz/issues/57)).

## mlr3viz 0.4.0

CRAN release: 2020-10-05

- All `autoplot.*()` functions now also have a generic S3
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) sibling
  ([\#51](https://github.com/mlr-org/mlr3viz/issues/51)).
- New plots for cluster tasks, learners and predictions from
  `mlr3cluster`.
- Fixed wrong labels for boxplots of `BenchmarkResult`.

## mlr3viz 0.3.0

CRAN release: 2020-09-25

- Compatibility with `mlr3` \>= 0.6.0.

## mlr3viz 0.2.0

CRAN release: 2020-08-07

- Added plots for `TaskDens` and `TaskSurv` from package `mlr3proba`.
- Update documentation of `PredictionRegr`
  ([\#23](https://github.com/mlr-org/mlr3viz/issues/23))
- [`autoplot.BenchmarkResult()`](https://mlr3viz.mlr-org.com/dev/reference/autoplot.BenchmarkResult.md):
  Support for learners with identical IDs
  ([\#19](https://github.com/mlr-org/mlr3viz/issues/19))
- Fixed a bug in
  [`plot_learner_prediction()`](https://mlr3viz.mlr-org.com/dev/reference/plot_learner_prediction.md)
  ([\#47](https://github.com/mlr-org/mlr3viz/issues/47))

## mlr3viz 0.1.1

CRAN release: 2020-02-19

- New plot: learner prediction for objects of class `ResampleResult`.
  Additionally, the helper function
  [`plot_learner_prediction()`](https://mlr3viz.mlr-org.com/dev/reference/plot_learner_prediction.md)
  first performs a
  [`resample()`](https://mlr3.mlr-org.com/reference/resample.html) and
  then plots the result.
- New plot: residual plot for objects of class `PredictionRegr`.

## mlr3viz 0.1.0

CRAN release: 2020-01-08

- Initial CRAN release
