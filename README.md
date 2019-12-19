
# mlr3viz

[![Travis build
status](https://travis-ci.org/mlr-org/mlr3viz.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3viz)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3viz)](https://cran.r-project.org/package=mlr3viz)
[![codecov](https://codecov.io/gh/mlr-org/mlr3viz/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3viz)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)

This R package provides visualizations for
[mlr3](https://mlr3.mlr-org.com) objects such as tasks, predictions,
resample results or benchmark results via the `autoplot()` generic of
[ggplot2](https://ggplot2.tidyverse.org/).

## Installation

CRAN version:

``` r
install.packages("mlr3filters")
```

Development version:

``` r
remotes::install_github("mlr-org/mlr3filters")
```

## Short Demo

``` r
library(mlr3)
library(mlr3viz)

task = tsk("pima")$select(c("age", "glucose", "insulin"))
learner = lrn("classif.rpart", predict_type = "prob")
rr = resample(task, learner, rsmp("cv", folds = 10))

# Default plot for task
autoplot(task)
```

![](man/figures/README-demo-1.png)<!-- -->

``` r
# Pairs plot from GGally
autoplot(task, type = "pairs")
```

![](man/figures/README-demo-2.png)<!-- -->

``` r
# ROC curve for the ResampleResult
autoplot(rr, type = "roc")
```

![](man/figures/README-demo-3.png)<!-- -->
