<<<<<<< HEAD
# mlr3viz 0.1.1.9003

- New plot: pairs plot for objects of class 'TaskClust'.
- New plot: scatter plot with cluster assignments for 
	objects of class 'PredictionClust'.

# mlr3viz 0.1.1.9002

- Update documentation of PredictionRegr (#23)

# mlr3viz 0.1.1.9001
=======
# mlr3viz 0.2.0
>>>>>>> upstream/master

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