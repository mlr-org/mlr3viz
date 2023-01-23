# mlr3viz 0.6.1

- Add `binwidth` argument to histogram plots.
- The `"performance"` plot always connected the maximum performance values.
  Now the minimum values are connected when the measure is minimized.

# mlr3viz 0.6.0

- Add `theme` option to `autoplot()` functions to supply a `ggplot2::theme()`.
  The default is `ggplot2::theme_minimal()`.
- Remove `theme_mlr3()`.
- Unify plot layouts.
- Remove support for passing extra arguments to `geom_` functions via `...`.
  This behavior was not consistent across the `autoplot()` functions.

# mlr3viz 0.5.10

- Improved documentation.
- Make checks run without suggested packages.

# mlr3viz 0.5.9

- Plots for survival objects moved to mlr3proba.

# mlr3viz 0.5.8

- Compatibility fixes.

# mlr3viz 0.5.7

- Compatibility fix for testthat.

# mlr3viz 0.5.6

- Compatibility fix for mlr3tuning.
- Fixed position of labels in barplot for `PredictionClassif`.


# mlr3viz 0.5.5

- Fixed another bug for ROC- and Precision-recall-curves (#79).

# mlr3viz 0.5.4

- Fixed a bug for ROC- and Precision-recall-curves (#72, #75).

# mlr3viz 0.5.3

- New autoplot for `LearnerClustHclust`.

# mlr3viz 0.5.2

- New autoplot for `TuningInstanceSingleCrit` from package `mlr3tuning`.
- Fixed bugs in autoplot function for `BenchmarkResult` (#63, #65).
- Fixed a bug in autoplot function for `PredictionClust` (#67).

# mlr3viz 0.5.1

- Fix test on solaris.


# mlr3viz 0.5.0

- New autoplot for `PredictionSurv`.
- New autoplots for learners from package `glmnet` via `ggfortify`.
- Fixed ROC and PRC plots for resampling `"holdout"` (#54).
- If possible, we now show confidence bounds for ROC and PRC plots (#55).
- Fixed a bug in autoplot function `TaskDens` (#57).


# mlr3viz 0.4.0

- All `autoplot.*()` functions now also have a generic S3 `plot()` sibling (#51).
- New plots for cluster tasks, learners and predictions from `mlr3cluster`.
- Fixed wrong labels for boxplots of `BenchmarkResult`.


# mlr3viz 0.3.0

- Compatibility with `mlr3` >= 0.6.0.


# mlr3viz 0.2.0

- Added plots for `TaskDens` and `TaskSurv` from package `mlr3proba`.
- Update documentation of `PredictionRegr` (#23)
- `autoplot.BenchmarkResult()`: Support for learners with identical IDs (#19)
- Fixed a bug in `plot_learner_prediction()` (#47)


# mlr3viz 0.1.1

- New plot: learner prediction for objects of class `ResampleResult`.
  Additionally, the helper function `plot_learner_prediction()` first performs a
  `resample()` and then plots the result.
- New plot: residual plot for objects of class `PredictionRegr`.


# mlr3viz 0.1.0

- Initial CRAN release
