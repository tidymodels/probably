# Uses a linear regression model to calibrate numeric predictions

Uses a linear regression model to calibrate numeric predictions

## Usage

``` r
cal_estimate_linear(
  .data,
  truth = NULL,
  estimate = dplyr::matches("^.pred$"),
  smooth = TRUE,
  parameters = NULL,
  ...,
  .by = NULL
)

# S3 method for class 'data.frame'
cal_estimate_linear(
  .data,
  truth = NULL,
  estimate = dplyr::matches("^.pred$"),
  smooth = TRUE,
  parameters = NULL,
  ...,
  .by = NULL
)

# S3 method for class 'tune_results'
cal_estimate_linear(
  .data,
  truth = NULL,
  estimate = dplyr::matches("^.pred$"),
  smooth = TRUE,
  parameters = NULL,
  ...
)

# S3 method for class 'grouped_df'
cal_estimate_linear(
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

  Am ungrouped `data.frame` object, or `tune_results` object, that
  contains a prediction column.

- truth:

  The column identifier for the observed outcome data (that is numeric).
  This should be an unquoted column name.

- estimate:

  Column identifier for the predicted values

- smooth:

  Applies to the linear models. It switches between a generalized
  additive model using spline terms when `TRUE`, and simple linear
  regression when `FALSE`.

- parameters:

  (Optional) An optional tibble of tuning parameter values that can be
  used to filter the predicted values before processing. Applies only to
  `tune_results` objects.

- ...:

  Additional arguments passed to the models or routines used to
  calculate the new predictions.

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

These methods estimate the relationship in the unmodified predicted
values and then remove that trend when
[`cal_apply()`](https://probably.tidymodels.org/dev/reference/cal_apply.md)
is invoked.

## See also

<https://www.tidymodels.org/learn/models/calibration/>,
[`cal_validate_linear()`](https://probably.tidymodels.org/dev/reference/cal_validate_linear.md)

## Examples

``` r
library(dplyr)
library(ggplot2)

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

# ------------------------------------------------------------------------------
# Before calibration

y_rng <- extendrange(boosting_predictions_test$outcome)

boosting_predictions_test |>
  ggplot(aes(outcome, .pred)) +
  geom_abline(lty = 2) +
  geom_point(alpha = 1 / 2) +
  geom_smooth(se = FALSE, col = "blue", linewidth = 1.2, alpha = 3 / 4) +
  coord_equal(xlim = y_rng, ylim = y_rng) +
  ggtitle("Before calibration")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# ------------------------------------------------------------------------------
# Smoothed trend removal

smoothed_cal <-
  boosting_predictions_oob |>
  # It will automatically identify the predicted value columns when the
  # standard tidymodels naming conventions are used.
  cal_estimate_linear(outcome)
smoothed_cal
#> 
#> ── Regression Calibration 
#> Method: Generalized additive model calibration
#> Source class: Data Frame
#> Data points: 2,000
#> Truth variable: `outcome`
#> Estimate variable: `.pred`

boosting_predictions_test |>
  cal_apply(smoothed_cal) |>
  ggplot(aes(outcome, .pred)) +
  geom_abline(lty = 2) +
  geom_point(alpha = 1 / 2) +
  geom_smooth(se = FALSE, col = "blue", linewidth = 1.2, alpha = 3 / 4) +
  coord_equal(xlim = y_rng, ylim = y_rng) +
  ggtitle("After calibration")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

```
