# Uses a logistic regression model to calibrate probabilities

Uses a logistic regression model to calibrate probabilities

## Usage

``` r
cal_estimate_logistic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred_"),
  smooth = TRUE,
  parameters = NULL,
  ...
)

# S3 method for class 'data.frame'
cal_estimate_logistic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred_"),
  smooth = TRUE,
  parameters = NULL,
  ...,
  .by = NULL
)

# S3 method for class 'tune_results'
cal_estimate_logistic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred_"),
  smooth = TRUE,
  parameters = NULL,
  ...
)

# S3 method for class 'grouped_df'
cal_estimate_logistic(
  .data,
  truth = NULL,
  estimate = NULL,
  smooth = TRUE,
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

- smooth:

  Applies to the logistic models. It switches between logistic spline
  when `TRUE`, and simple logistic regression when `FALSE`.

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

This function uses existing modeling functions from other packages to
create the calibration:

- [`stats::glm()`](https://rdrr.io/r/stats/glm.html) is used when
  `smooth` is set to `FALSE`

- [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) is used when
  `smooth` is set to `TRUE`

### Multiclass Extension

This method has *not* been extended to multiclass outcomes. However, the
natural multiclass extension is
[`cal_estimate_multinomial()`](https://probably.tidymodels.org/dev/reference/cal_estimate_multinomial.md).

## See also

<https://www.tidymodels.org/learn/models/calibration/>,
[`cal_validate_logistic()`](https://probably.tidymodels.org/dev/reference/cal_validate_logistic.md)

## Examples

``` r
# It will automatically identify the probability columns
# if passed a model fitted with tidymodels
cal_estimate_logistic(segment_logistic, Class)
#> 
#> ── Probability Calibration 
#> Method: Generalized additive model calibration
#> Type: Binary
#> Source class: Data Frame
#> Data points: 1,010
#> Truth variable: `Class`
#> Estimate variables:
#> `.pred_good` ==> good
#> `.pred_poor` ==> poor

# Specify the variable names in a vector of unquoted names
cal_estimate_logistic(segment_logistic, Class, c(.pred_poor, .pred_good))
#> 
#> ── Probability Calibration 
#> Method: Generalized additive model calibration
#> Type: Binary
#> Source class: Data Frame
#> Data points: 1,010
#> Truth variable: `Class`
#> Estimate variables:
#> `.pred_good` ==> good
#> `.pred_poor` ==> poor

# dplyr selector functions are also supported
cal_estimate_logistic(segment_logistic, Class, dplyr::starts_with(".pred_"))
#> 
#> ── Probability Calibration 
#> Method: Generalized additive model calibration
#> Type: Binary
#> Source class: Data Frame
#> Data points: 1,010
#> Truth variable: `Class`
#> Estimate variables:
#> `.pred_good` ==> good
#> `.pred_poor` ==> poor
```
