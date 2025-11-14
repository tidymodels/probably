# Prediction intervals via conformal inference and quantile regression

Nonparametric prediction intervals can be computed for fitted regression
workflow objects using the split conformal inference method described by
Romano *et al* (2019). To compute quantiles, this function uses Quantile
Random Forests instead of classic quantile regression.

## Usage

``` r
int_conformal_quantile(object, ...)

# S3 method for class 'workflow'
int_conformal_quantile(object, train_data, cal_data, level = 0.95, ...)
```

## Arguments

- object:

  A fitted
  [`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
  object.

- ...:

  Options to pass to
  [`quantregForest::quantregForest()`](https://rdrr.io/pkg/quantregForest/man/quantregForest.html)
  (such as the number of trees).

- train_data, cal_data:

  Data frames with the *predictor and outcome data*. `train_data` should
  be the same data used to produce `object` and `cal_data` is used to
  produce predictions (and residuals). If the workflow used a recipe,
  these should be the data that were inputs to the recipe (and not the
  product of a recipe).

- level:

  The confidence level for the intervals.

## Value

An object of class `"int_conformal_quantile"` containing the information
to create intervals (which includes `object`). The
[`predict()`](https://rdrr.io/r/stats/predict.html) method is used to
produce the intervals.

## Details

Note that the significance level should be specified in this function
(instead of the [`predict()`](https://rdrr.io/r/stats/predict.html)
method).

`cal_data` should be large enough to get a good estimates of a extreme
quantile (e.g., the 95th for 95% interval) and should not include rows
that were in the original training set.

Note that the because of the method used to construct the interval, it
is possible that the prediction intervals will not include the predicted
value.

## References

Romano, Yaniv, Evan Patterson, and Emmanuel Candes. "Conformalized
quantile regression." *Advances in neural information processing
systems* 32 (2019).

## See also

[`predict.int_conformal_quantile()`](https://probably.tidymodels.org/dev/reference/predict.int_conformal_full.md)

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

mlp_int <- int_conformal_quantile(mlp_fit, sim_train, sim_cal,
  level = 0.90
)
mlp_int
#> Split Conformal inference via Quantile Regression
#> preprocessor: formula 
#> model: mlp (engine = nnet) 
#> calibration set size: 200 
#> confidence level: 0.9 
#> 
#> Use `predict(object, new_data)` to compute prediction intervals

predict(mlp_int, sim_new)
#> # A tibble: 5 × 3
#>   .pred .pred_lower .pred_upper
#>   <dbl>       <dbl>       <dbl>
#> 1  4.46      -14.1         29.7
#> 2  5.83      -14.1         50.2
#> 3  9.27      -11.3         36.1
#> 4  1.50        1.67        46.8
#> 5  9.68      -15.6         19.8
```
