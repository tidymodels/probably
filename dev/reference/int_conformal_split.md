# Prediction intervals via split conformal inference

Nonparametric prediction intervals can be computed for fitted regression
workflow objects using the split conformal inference method described by
Lei *et al* (2018).

## Usage

``` r
int_conformal_split(object, ...)

# Default S3 method
int_conformal_split(object, ...)

# S3 method for class 'workflow'
int_conformal_split(object, cal_data, ...)
```

## Arguments

- object:

  A fitted
  [`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
  object.

- ...:

  Not currently used.

- cal_data:

  A data frame with the *original predictor and outcome data* used to
  produce predictions (and residuals). If the workflow used a recipe,
  this should be the data that were inputs to the recipe (and not the
  product of a recipe).

## Value

An object of class `"int_conformal_split"` containing the information to
create intervals (which includes `object`). The
[`predict()`](https://rdrr.io/r/stats/predict.html) method is used to
produce the intervals.

## Details

This function implements what is usually called "split conformal
inference" (see Algorithm 1 in Lei *et al* (2018)).

This function prepares the statistics for the interval computations. The
[`predict()`](https://rdrr.io/r/stats/predict.html) method computes the
intervals for new data and the signficance level is specified there.

`cal_data` should be large enough to get a good estimates of a extreme
quantile (e.g., the 95th for 95% interval) and should not include rows
that were in the original training set.

## References

Lei, Jing, et al. "Distribution-free predictive inference for
regression." *Journal of the American Statistical Association* 113.523
(2018): 1094-1111.

## See also

[`predict.int_conformal_split()`](https://probably.tidymodels.org/dev/reference/predict.int_conformal_full.md)

## Examples

``` r
library(workflows)
library(dplyr)
library(parsnip)
library(rsample)
library(tune)
library(modeldata)

set.seed(2)
sim_train <- sim_regression(500)
sim_cal <- sim_regression(200)
sim_new <- sim_regression(5) |> select(-outcome)

# We'll use a neural network model
mlp_spec <-
  mlp(hidden_units = 5, penalty = 0.01) |>
  set_mode("regression")

mlp_wflow <-
  workflow() |>
  add_model(mlp_spec) |>
  add_formula(outcome ~ .)

mlp_fit <- fit(mlp_wflow, data = sim_train)

mlp_int <- int_conformal_split(mlp_fit, sim_cal)
mlp_int
#> Split Conformal inference
#> preprocessor: formula 
#> model: mlp (engine = nnet) 
#> calibration set size: 200 
#> 
#> Use `predict(object, new_data, level)` to compute prediction intervals

predict(mlp_int, sim_new, level = 0.90)
#> # A tibble: 5 × 3
#>   .pred .pred_lower .pred_upper
#>   <dbl>       <dbl>       <dbl>
#> 1  4.46       -27.5        36.4
#> 2  5.83       -26.1        37.8
#> 3  9.27       -22.7        41.2
#> 4  1.50       -30.4        33.4
#> 5  9.68       -22.3        41.6
```
