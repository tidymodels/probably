# Do not calibrate model predictions.

Do not calibrate model predictions.

## Usage

``` r
cal_estimate_none(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...
)

# S3 method for class 'data.frame'
cal_estimate_none(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...,
  .by = NULL
)

# S3 method for class 'tune_results'
cal_estimate_none(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...
)

# S3 method for class 'grouped_df'
cal_estimate_none(.data, truth = NULL, estimate = NULL, parameters = NULL, ...)
```

## Arguments

- .data:

  An ungrouped `data.frame` object, or `tune_results` object, that
  contains predictions and probability columns.

- truth:

  The column identifier for the true outcome results (that is factor or
  numeric). This should be an unquoted column name.

- estimate:

  A vector of column identifiers, or one of `dplyr` selector functions
  to choose which variables contains the class probabilities or numeric
  predictions. It defaults to the prefix used by tidymodels (`.pred_`).
  For classification problems, the order of the identifiers will be
  considered the same as the order of the levels of the `truth`
  variable.

- parameters:

  (Optional) An optional tibble of tuning parameter values that can be
  used to filter the predicted values before processing. Applies only to
  `tune_results` objects.

- ...:

  Additional arguments passed to the models or routines used to
  calculate the new probabilities.

- .by:

  The column identifier for the grouping variable. This should be a
  single unquoted column name that selects a qualitative variable for
  grouping. Default to `NULL`. When `.by = NULL` no grouping will take
  place.

## Details

This function does nothing to the predictions. It is used as a reference
when tuning over different calibration methods.

## Examples

``` r
nada <- cal_estimate_none(boosting_predictions_oob, outcome, .pred)
nada
#> 
#> ── Regression Calibration 
#> Method: No calibration
#> Source class: Data Frame
#> Data points: 2,000
#> Truth variable: `outcome`
#> Estimate variable: `.pred`

identical(
  cal_apply(boosting_predictions_oob, nada),
  boosting_predictions_oob
)
#> [1] TRUE

# ------------------------------------------------------------------------------

nichts <- cal_estimate_none(segment_logistic, Class)

identical(
  cal_apply(segment_logistic, nichts),
  segment_logistic
)
#> [1] TRUE
```
