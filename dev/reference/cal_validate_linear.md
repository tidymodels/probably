# Measure performance with and without using linear regression calibration

Measure performance with and without using linear regression calibration

## Usage

``` r
cal_validate_linear(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  metrics = NULL,
  save_pred = FALSE,
  ...
)

# S3 method for class 'resample_results'
cal_validate_linear(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  metrics = NULL,
  save_pred = FALSE,
  ...
)

# S3 method for class 'rset'
cal_validate_linear(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  metrics = NULL,
  save_pred = FALSE,
  ...
)
```

## Arguments

- .data:

  An `rset` object or the results of
  [`tune::fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html)
  with a `.predictions` column.

- truth:

  The column identifier for the true class results (that is a factor).
  This should be an unquoted column name.

- estimate:

  A vector of column identifiers, or one of `dplyr` selector functions
  to choose which variables contains the class probabilities. It
  defaults to the prefix used by tidymodels (`.pred_`). The order of the
  identifiers will be considered the same as the order of the levels of
  the `truth` variable.

- metrics:

  A set of metrics passed created via
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)

- save_pred:

  Indicates whether to a column of post-calibration predictions.

- ...:

  Options to pass to
  [`cal_estimate_logistic()`](https://probably.tidymodels.org/dev/reference/cal_estimate_logistic.md),
  such as the `smooth` argument.

## Performance Metrics

By default, the average of the root mean square error (RMSE) is
returned. Any appropriate
[`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
can be used. The validation function compares the average of the metrics
before, and after the calibration.

## See also

<https://www.tidymodels.org/learn/models/calibration/>,
[`cal_estimate_linear()`](https://probably.tidymodels.org/dev/reference/cal_estimate_linear.md)

## Examples

``` r
library(dplyr)
library(yardstick)
library(rsample)

head(boosting_predictions_test)
#> # A tibble: 6 × 2
#>   outcome .pred
#>     <dbl> <dbl>
#> 1   -4.65  4.12
#> 2    1.12  1.83
#> 3   14.7  13.1 
#> 4   36.3  19.1 
#> 5   14.1  14.9 
#> 6   -4.22  8.10

reg_stats <- metric_set(rmse, ccc)

set.seed(828)
boosting_predictions_oob |>
  # Resample with 10-fold cross-validation
  vfold_cv() |>
  cal_validate_linear(truth = outcome, smooth = FALSE, metrics = reg_stats)
#> #  10-fold cross-validation 
#> # A tibble: 10 × 4
#>    splits             id     .metrics         .metrics_cal    
#>    <list>             <chr>  <list>           <list>          
#>  1 <split [1800/200]> Fold01 <tibble [2 × 3]> <tibble [2 × 3]>
#>  2 <split [1800/200]> Fold02 <tibble [2 × 3]> <tibble [2 × 3]>
#>  3 <split [1800/200]> Fold03 <tibble [2 × 3]> <tibble [2 × 3]>
#>  4 <split [1800/200]> Fold04 <tibble [2 × 3]> <tibble [2 × 3]>
#>  5 <split [1800/200]> Fold05 <tibble [2 × 3]> <tibble [2 × 3]>
#>  6 <split [1800/200]> Fold06 <tibble [2 × 3]> <tibble [2 × 3]>
#>  7 <split [1800/200]> Fold07 <tibble [2 × 3]> <tibble [2 × 3]>
#>  8 <split [1800/200]> Fold08 <tibble [2 × 3]> <tibble [2 × 3]>
#>  9 <split [1800/200]> Fold09 <tibble [2 × 3]> <tibble [2 × 3]>
#> 10 <split [1800/200]> Fold10 <tibble [2 × 3]> <tibble [2 × 3]>
```
