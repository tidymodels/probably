# Prediction intervals via conformal inference CV+

Nonparametric prediction intervals can be computed for fitted regression
workflow objects using the CV+ conformal inference method described by
Barber *at al* (2018).

## Usage

``` r
int_conformal_cv(object, ...)

# Default S3 method
int_conformal_cv(object, ...)

# S3 method for class 'resample_results'
int_conformal_cv(object, ...)

# S3 method for class 'tune_results'
int_conformal_cv(object, parameters, ...)
```

## Arguments

- object:

  An object from a tidymodels resampling or tuning function such as
  [`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html),
  [`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
  or similar. The object should have been produced in a way that the
  `.extracts` column contains the fitted workflow for each resample (see
  the Details below).

- ...:

  Not currently used.

- parameters:

  An tibble of tuning parameter values that can be used to filter the
  predicted values before processing. This tibble should select a single
  set of hyper-parameter values from the tuning results. This is only
  required when a tuning object is passed to `object`.

## Value

An object of class `"int_conformal_cv"` containing the information to
create intervals. The
[`predict()`](https://rdrr.io/r/stats/predict.html) method is used to
produce the intervals.

## Details

This function implements the CV+ method found in Section 3 of Barber *at
al* (2018). It uses the resampled model fits and their associated
holdout residuals to make prediction intervals for regression models.

This function prepares the objects for the computations. The
[`predict()`](https://rdrr.io/r/stats/predict.html) method computes the
intervals for new data.

This method was developed for V-fold cross-validation (no repeats).
Interval coverage is unknown for any other resampling methods. The
function will not stop the computations for other types of resamples,
but we have no way of knowing whether the results are appropriate.

## References

Rina Foygel Barber, Emmanuel J. Candès, Aaditya Ramdas, Ryan J.
Tibshirani "Predictive inference with the jackknife+," *The Annals of
Statistics*, 49(1), 486-507, 2021

## See also

[`predict.int_conformal_cv()`](https://probably.tidymodels.org/dev/reference/predict.int_conformal_full.md)

## Examples

``` r
library(workflows)
library(dplyr)
library(parsnip)
library(rsample)
library(tune)
library(modeldata)

set.seed(2)
sim_train <- sim_regression(200)
sim_new <- sim_regression(5) |> select(-outcome)

sim_rs <- vfold_cv(sim_train)

# We'll use a neural network model
mlp_spec <-
  mlp(hidden_units = 5, penalty = 0.01) |>
  set_mode("regression")

# Use a control function that saves the predictions as well as the models.
# Consider using the butcher package in the extracts function to have smaller
# object sizes

ctrl <- control_resamples(save_pred = TRUE, extract = I)

set.seed(3)
nnet_res <-
  mlp_spec |>
  fit_resamples(outcome ~ ., resamples = sim_rs, control = ctrl)

nnet_int_obj <- int_conformal_cv(nnet_res)
nnet_int_obj
#> Conformal inference via CV+
#> preprocessor: formula 
#> model: mlp (engine = nnet) 
#> number of models: 10 
#> training set size: 200 
#> 
#> Use `predict(object, new_data, level)` to compute prediction intervals

predict(nnet_int_obj, sim_new)
#> # A tibble: 5 × 3
#>   .pred_lower .pred .pred_upper
#>         <dbl> <dbl>       <dbl>
#> 1      0.0941 42.6         85.2
#> 2    -29.3    13.2         55.8
#> 3    -22.2    20.3         62.8
#> 4    -45.4    -2.85        39.7
#> 5    -12.9    29.6         72.1
```
