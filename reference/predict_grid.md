# Generates a data.table of evenly distributed points.

For each point we have the predicted class / regression value in column
response. If the learner predicts probabilities, a column
".prob.response" is added that contains the probability of the predicted
class

## Usage

``` r
predict_grid(learners, task, grid_points, expand_range)
```

## Arguments

- learners:

  list of trained learners, each learner belongs to one resampling
  iteration

- task:

  the task all learners are trained on

- grid_points:

  (int): see sequenize

- expand_range:

  see sequenize
