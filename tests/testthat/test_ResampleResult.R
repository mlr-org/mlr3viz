task = mlr3::tsk("sonar")
learner = mlr3::lrn("classif.rpart", predict_type = "prob")
resampling = mlr3::rsmp("cv")
rr = mlr3::resample(task, learner, resampling)

test_that("fortify ResampleResult", {
  f = fortify(rr, measure = msr("classif.ce"))
  expect_data_table(f, nrows = 10, ncols = 3)
  expect_names(names(f), identical.to = c(
    "iteration", "measure_id",
    "performance"))
})

test_that("autoplot ResampleResult", {
  p = autoplot(rr, measure = msr("classif.ce"), type = "boxplot")
  expect_true(is.ggplot(p))

  p = autoplot(rr, measure = msr("classif.ce"), type = "histogram")
  expect_true(is.ggplot(p))

  p = autoplot(rr, type = "roc")
  expect_true(is.ggplot(p))

  p = autoplot(rr, type = "prc")
  expect_true(is.ggplot(p))
})

test_that("autoplot ResampleResult type=prediction", {
  tasks = list(
    classif_2d = mlr3::tsk("iris")$select(c("Sepal.Length", "Sepal.Width")),
    regr_1d = mlr3::tsk("boston_housing")$select("age"),
    regr_2d = mlr3::tsk("boston_housing")$select(c("age", "chas"))
  )

  predict_sets = list(character(), "train", "test", c("train", "test"))

  resamplings = list(rsmp("cv", folds = 3), rsmp("bootstrap", repeats = 3))

  for (task in tasks) {
    if (task$task_type == "classif") {
      learners = lapply(c("response", "prob"), function(x) {
        lrn("classif.featureless", predict_type = x)
      })
    } else {
      learners = lapply(c("response", "se"), function(x) {
        lrn("regr.featureless", predict_type = x)
      })
    }
    for (learner in learners) {
      for (resampling in resamplings) {
        rr = resample(task, learner, resampling, store_models = TRUE)
        for (predict_set in predict_sets) {
          p = autoplot(rr, type = "prediction", predict_sets = predict_set)
          expect_true(is.ggplot(p))
        }
      }
    }
  }

  # check errors
  rr = resample(mlr3::tsk("iris")$select(c("Sepal.Length", "Sepal.Width")),
    lrn("classif.featureless"), resampling,
    store_models = FALSE)
  expect_error(autoplot(rr, type = "prediction"), regexp = "store_models")
  rr = resample(mlr3::tsk("iris"), lrn("classif.featureless"), resampling,
    store_models = TRUE)
  expect_error(autoplot(rr, type = "prediction"),
    regexp = "only works for tasks with two features")
  rr = resample(mlr3::tsk("boston_housing"), lrn("regr.featureless"), resampling,
    store_models = TRUE)
  expect_error(autoplot(rr, type = "prediction"),
    regexp = "Plot learner prediction only works with one or two features for
regression!")
})


test_that("roc is not inverted", {
  autoplot(rr, type = "roc")
  skip_if_not_installed("precrec")
  tab = as.data.table(precrec::auc(precrec::evalmod(as_precrec(rr))))
  expect_number(mean(tab[curvetypes == "ROC", aucs]), lower = 0.5)
})
