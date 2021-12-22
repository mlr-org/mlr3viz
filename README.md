
# mlr3viz

Package website: [release](https://mlr3viz.mlr-org.com/) \|
[dev](https://mlr3viz.mlr-org.com/dev/)

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3viz/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3viz/actions)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3viz)](https://cran.r-project.org/package=mlr3viz)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

This R package provides visualizations for
[mlr3](https://mlr3.mlr-org.com) objects such as tasks, predictions,
resample results or benchmark results via the `autoplot()` generic of
[ggplot2](https://ggplot2.tidyverse.org/).

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3viz")
```

## Short Demo

``` r
library(mlr3)
library(mlr3viz)

task = tsk("iris")$select(c("Sepal.Length", "Sepal.Width"))
learner = lrn("classif.rpart", predict_type = "prob")
rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)

# Default plot for task
autoplot(task)
```

![](man/figures/README-demo-1.png)<!-- -->

``` r
# Advanced resample result prediction plot
autoplot(rr, type = "prediction")
```

![](man/figures/README-demo-2.png)<!-- -->

For more examples plots you can have a look at the [pkgdown
references](https://mlr3viz.mlr-org.com/reference/index.html) of the
respective functions.

## Theming

{mlr3viz} styles all plots with it’s own theme `theme_mlr3()` (which is
heavily influenced by the `ggpubr::theme_pubr()` theme) and the
“viridis” color palette. If you want to use a different theme or color
palette, apply it after the `autoplot()` call as in

``` r
autoplot(<object>) +
  scale_color_discrete() +
  theme_gray()
```

For color scheme adjustments you might need to change `*_color_*` to
`*_fill_*` or `*_*_discrete` to `*_*_cotinuous`, depending on the object
that was visualized.

For even more control, you can look up the source code which ggplot2
geoms were used internally for a specific `autoplot()` call
(e.g. `geom_point()`) and how they were called. You can then apply these
lines again with different arguments after the `autoplot()` call
(similar as shown above with the `theme_gray()` adjustment) to overwrite
their appearance (for example point size, line width, etc.).
