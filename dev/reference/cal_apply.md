# Applies a calibration to a set of existing predictions

Applies a calibration to a set of existing predictions

## Usage

``` r
cal_apply(.data, object, pred_class = NULL, parameters = NULL, ...)

# S3 method for class 'data.frame'
cal_apply(.data, object, pred_class = NULL, parameters = NULL, ...)

# S3 method for class 'tune_results'
cal_apply(.data, object, pred_class = NULL, parameters = NULL, ...)

# S3 method for class 'cal_object'
cal_apply(.data, object, pred_class = NULL, parameters = NULL, ...)
```

## Arguments

- .data:

  An object that can process a calibration object.

- object:

  The calibration object (`cal_object`).

- pred_class:

  (Optional, classification only) Column identifier for the hard class
  predictions (a factor vector). This column will be adjusted based on
  changes to the calibrated probability columns.

- parameters:

  (Optional) An optional tibble of tuning parameter values that can be
  used to filter the predicted values before processing. Applies only to
  `tune_results` objects.

- ...:

  Optional arguments; currently unused.

## Details

`cal_apply()` currently supports data.frames only. It extracts the
`truth` and the estimate columns names from the calibration object.

## See also

<https://www.tidymodels.org/learn/models/calibration/>,
[`cal_estimate_beta()`](https://probably.tidymodels.org/dev/reference/cal_estimate_beta.md),
[`cal_estimate_isotonic()`](https://probably.tidymodels.org/dev/reference/cal_estimate_isotonic.md),
[`cal_estimate_isotonic_boot()`](https://probably.tidymodels.org/dev/reference/cal_estimate_isotonic_boot.md),
[`cal_estimate_linear()`](https://probably.tidymodels.org/dev/reference/cal_estimate_linear.md),
[`cal_estimate_logistic()`](https://probably.tidymodels.org/dev/reference/cal_estimate_logistic.md),
[`cal_estimate_multinomial()`](https://probably.tidymodels.org/dev/reference/cal_estimate_multinomial.md)

## Examples

``` r
# ------------------------------------------------------------------------------
# classification example

w_calibration <- cal_estimate_logistic(segment_logistic, Class)
#> Registered S3 method overwritten by 'butcher':
#>   method                 from    
#>   as.character.dev_topic generics

cal_apply(segment_logistic, w_calibration)
#> # A tibble: 1,010 × 3
#>    .pred_poor .pred_good Class
#>         <dbl>      <dbl> <fct>
#>  1      0.974     0.0258 poor 
#>  2      0.930     0.0700 poor 
#>  3      0.220     0.780  good 
#>  4      0.205     0.795  good 
#>  5      0.976     0.0244 poor 
#>  6      0.590     0.410  good 
#>  7      0.777     0.223  good 
#>  8      0.135     0.865  good 
#>  9      0.977     0.0231 poor 
#> 10      0.770     0.230  poor 
#> # ℹ 1,000 more rows
```
