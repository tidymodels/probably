# Uses a Beta calibration model to calculate new probabilities

Uses a Beta calibration model to calculate new probabilities

## Usage

``` r
cal_estimate_beta(
  .data,
  truth = NULL,
  shape_params = 2,
  location_params = 1,
  estimate = dplyr::starts_with(".pred_"),
  parameters = NULL,
  ...
)

# S3 method for class 'data.frame'
cal_estimate_beta(
  .data,
  truth = NULL,
  shape_params = 2,
  location_params = 1,
  estimate = dplyr::starts_with(".pred_"),
  parameters = NULL,
  ...,
  .by = NULL
)

# S3 method for class 'tune_results'
cal_estimate_beta(
  .data,
  truth = NULL,
  shape_params = 2,
  location_params = 1,
  estimate = dplyr::starts_with(".pred_"),
  parameters = NULL,
  ...
)

# S3 method for class 'grouped_df'
cal_estimate_beta(
  .data,
  truth = NULL,
  shape_params = 2,
  location_params = 1,
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

- shape_params:

  Number of shape parameters to use. Accepted values are 1 and 2.
  Defaults to 2.

- location_params:

  Number of location parameters to use. Accepted values 1 and 0.
  Defaults to 1.

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

This function uses the
[`betacal::beta_calibration()`](https://rdrr.io/pkg/betacal/man/beta_calibration.html)
function, and retains the resulting model.

## Multiclass Extension

This method is designed to work with two classes. For multiclass, it
creates a set of "one versus all" calibrations for each class. After
they are applied to the data, the probability estimates are
re-normalized to add to one. This final step might compromise the
calibration.

## References

Meelis Kull, Telmo M. Silva Filho, Peter Flach "Beyond sigmoids: How to
obtain well-calibrated probabilities from binary classifiers with beta
calibration," *Electronic Journal of Statistics* 11(2), 5052-5080,
(2017)

## See also

<https://www.tidymodels.org/learn/models/calibration/>,
[`cal_validate_beta()`](https://probably.tidymodels.org/dev/reference/cal_validate_beta.md)

## Examples

``` r
if (rlang::is_installed("betacal")) {
 # It will automatically identify the probability columns
  # if passed a model fitted with tidymodels
  cal_estimate_beta(segment_logistic, Class)
}
#> 
#> ── Probability Calibration 
#> Method: Beta calibration
#> Type: Binary
#> Source class: Data Frame
#> Data points: 1,010
#> Truth variable: `Class`
#> Estimate variables:
#> `.pred_good` ==> good
#> `.pred_poor` ==> poor
```
