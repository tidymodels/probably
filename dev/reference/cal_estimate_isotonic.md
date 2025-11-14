# Uses an Isotonic regression model to calibrate model predictions.

Uses an Isotonic regression model to calibrate model predictions.

## Usage

``` r
cal_estimate_isotonic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...
)

# S3 method for class 'data.frame'
cal_estimate_isotonic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...,
  .by = NULL
)

# S3 method for class 'tune_results'
cal_estimate_isotonic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...
)

# S3 method for class 'grouped_df'
cal_estimate_isotonic(
  .data,
  truth = NULL,
  estimate = NULL,
  parameters = NULL,
  ...
)
```

## Arguments

- .data:

  An ungrouped `data.frame` object, or `tune_results` object, that
  contains predictions and probability columns.

- truth:

  The column identifier for the true class results (that is a factor).
  This should be an unquoted column name.

- estimate:

  A vector of column identifiers, or one of `dplyr` selector functions
  to choose which variables contains the class probabilities. It
  defaults to the prefix used by tidymodels (`.pred_`). The order of the
  identifiers will be considered the same as the order of the levels of
  the `truth` variable.

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

This function uses
[`stats::isoreg()`](https://rdrr.io/r/stats/isoreg.html) to create
obtain the calibration values for binary classification or numeric
regression.

## Multiclass Extension

This method is designed to work with two classes. For multiclass, it
creates a set of "one versus all" calibrations for each class. After
they are applied to the data, the probability estimates are
re-normalized to add to one. This final step might compromise the
calibration.

## References

Zadrozny, Bianca and Elkan, Charles. (2002). Transforming Classifier
Scores into Accurate Multiclass Probability Estimates. *Proceedings of
the ACM SIGKDD International Conference on Knowledge Discovery and Data
Mining.*

## See also

<https://www.tidymodels.org/learn/models/calibration/>,
[`cal_validate_isotonic()`](https://probably.tidymodels.org/dev/reference/cal_validate_isotonic.md)

## Examples

``` r
# ------------------------------------------------------------------------------
# Binary Classification

# It will automatically identify the probability columns
# if passed a model fitted with tidymodels
cal_estimate_isotonic(segment_logistic, Class)
#> 
#> ── Probability Calibration 
#> Method: Isotonic regression calibration
#> Type: Binary
#> Source class: Data Frame
#> Data points: 1,010
#> Unique Predicted Values: 78
#> Truth variable: `Class`
#> Estimate variables:
#> `.pred_good` ==> good
#> `.pred_poor` ==> poor

# Specify the variable names in a vector of unquoted names
cal_estimate_isotonic(segment_logistic, Class, c(.pred_poor, .pred_good))
#> 
#> ── Probability Calibration 
#> Method: Isotonic regression calibration
#> Type: Binary
#> Source class: Data Frame
#> Data points: 1,010
#> Unique Predicted Values: 78
#> Truth variable: `Class`
#> Estimate variables:
#> `.pred_good` ==> good
#> `.pred_poor` ==> poor

# dplyr selector functions are also supported
cal_estimate_isotonic(segment_logistic, Class, dplyr::starts_with(".pred_"))
#> 
#> ── Probability Calibration 
#> Method: Isotonic regression calibration
#> Type: Binary
#> Source class: Data Frame
#> Data points: 1,010
#> Unique Predicted Values: 78
#> Truth variable: `Class`
#> Estimate variables:
#> `.pred_good` ==> good
#> `.pred_poor` ==> poor

# ------------------------------------------------------------------------------
# Regression (numeric outcomes)

cal_estimate_isotonic(boosting_predictions_oob, outcome, .pred)
#> 
#> ── Probability Calibration 
#> Method: Isotonic regression calibration
#> Type: Regression
#> Source class: Data Frame
#> Data points: 2,000
#> Unique Predicted Values: 47
#> Truth variable: `outcome`
#> Estimate variables:
#> `.pred` ==> predictions
```
