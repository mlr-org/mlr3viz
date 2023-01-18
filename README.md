
# mlr3viz <img src="man/figures/logo.png" align="right" width = "120" />

Package website: [release](https://mlr3viz.mlr-org.com/) |
[dev](https://mlr3viz.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3viz/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3viz/actions/workflows/r-cmd-check.yml)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3viz)](https://cran.r-project.org/package=mlr3viz)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

*mlr3viz* is the visualization package of the
[mlr3](https://mlr-org.com/) ecosystem. It features plots for mlr3
objects such as tasks, learners, predictions, benchmark results, tuning
instances and filters via the `autoplot()` generic of
[ggplot2](https://ggplot2.tidyverse.org/). The package draws plots with
the [viridis](https://CRAN.R-project.org/package=viridisLite) color
palette and applies the [minimal
theme](https://ggplot2.tidyverse.org/reference/ggtheme.html).
Visualizations include barplots, boxplots, histograms, ROC curves, and
Precision-Recall curves.

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3viz")
```

## Resources

The [gallery](https://mlr-org.com/gallery/technical/2022-12-22-mlr3viz/)
features a showcase post of the visualization functions mlr3viz.

## Short Demo

``` r
library(mlr3)
library(mlr3viz)

task = tsk("pima")
learner = lrn("classif.rpart", predict_type = "prob")
rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)

# Default plot for task
autoplot(task, type = "target")
```

![](man/figures/README-demo-1.png)<!-- -->

``` r
# ROC curve for resample result
autoplot(rr, type = "roc")
```

![](man/figures/README-demo-2.png)<!-- -->

For more example plots you can have a look at the [pkgdown
references](https://mlr3viz.mlr-org.com/reference/index.html) of the
respective functions.
