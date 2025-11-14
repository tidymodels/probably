# Prediction intervals from conformal methods

Prediction intervals from conformal methods

## Usage

``` r
# S3 method for class 'int_conformal_cv'
predict(object, new_data, level = 0.95, ...)

# S3 method for class 'int_conformal_full'
predict(object, new_data, level = 0.95, ...)

# S3 method for class 'int_conformal_quantile'
predict(object, new_data, ...)

# S3 method for class 'int_conformal_split'
predict(object, new_data, level = 0.95, ...)
```

## Arguments

- object:

  An object produced by `predict.int_conformal_full()`.

- new_data:

  A data frame of predictors.

- level:

  The confidence level for the intervals.

- ...:

  Not currently used.

## Value

A tibble with columns `.pred_lower` and `.pred_upper`. If the
computations for the prediction bound fail, a missing value is used. For
objects produced by
[`int_conformal_cv()`](https://probably.tidymodels.org/dev/reference/int_conformal_cv.md),
an additional `.pred` column is also returned (see Details below).

## Details

For the CV+. estimator produced by
[`int_conformal_cv()`](https://probably.tidymodels.org/dev/reference/int_conformal_cv.md),
the intervals are centered around the mean of the predictions produced
by the resample-specific model. For example, with 10-fold
cross-validation, `.pred` is the average of the predictions from the 10
models produced by each fold. This may differ from the prediction
generated from a model fit that was trained on the entire training set,
especially if the training sets are small.

## See also

[`int_conformal_full()`](https://probably.tidymodels.org/dev/reference/int_conformal_full.md),
[`int_conformal_cv()`](https://probably.tidymodels.org/dev/reference/int_conformal_cv.md)
