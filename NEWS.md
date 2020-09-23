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
