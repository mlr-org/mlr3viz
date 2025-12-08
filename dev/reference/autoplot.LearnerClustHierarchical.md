# Plots for Hierarchical Clustering Learners

Visualizations for hierarchical clusters. The argument `type` controls
what kind of plot is drawn. Possible choices are:

- `"dend"` (default): Dendrograms using
  [ggdendro](https://CRAN.R-project.org/package=ggdendro) package.

- `"scree"`: Scree plot that shows the number of possible clusters on
  the x-axis and the height on the y-axis.

## Usage

``` r
# S3 method for class 'LearnerClustHierarchical'
autoplot(
  object,
  type = "dend",
  task = NULL,
  theme = theme_minimal(),
  theme_dendro = TRUE,
  ...
)
```

## Arguments

- object:

  ([mlr3cluster::LearnerClustAgnes](https://mlr3cluster.mlr-org.com/reference/mlr_learners_clust.agnes.html)
  \|
  [mlr3cluster::LearnerClustDiana](https://mlr3cluster.mlr-org.com/reference/mlr_learners_clust.diana.html)
  \|
  [mlr3cluster::LearnerClustHclust](https://mlr3cluster.mlr-org.com/reference/mlr_learners_clust.hclust.html)).

- type:

  (character(1)):  
  Type of the plot. See description.

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Optionally, pass the task to add labels of observations to a `hclust`
  dendrogram. Labels are set via the row names of the task.

- theme:

  ([`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html))  
  The
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  is applied by default to all plots.

- theme_dendro:

  (`logical(1)`)  
  If `TRUE` (default), the special dendrogram theme from
  [ggdendro](https://CRAN.R-project.org/package=ggdendro) package is
  used in plot `"dend"`. Set to `FALSE` to use the theme passed in
  `theme`.

- ...:

  (ignored).

## Value

[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
if (requireNamespace("mlr3")) {
  library(mlr3)
  library(mlr3cluster)
  library(mlr3viz)

  task = tsk("usarrests")

  # agnes clustering
  learner = lrn("clust.agnes")
  learner$train(task)
  autoplot(learner)

  # diana clustering
  learner = lrn("clust.diana")
  learner$train(task)
  autoplot(learner)

  # hclust clustering
  learner = lrn("clust.hclust")
  learner$train(task)
  autoplot(learner, type = "scree")
}
```
