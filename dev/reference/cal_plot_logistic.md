# Probability calibration plots via logistic regression

A logistic regression model is fit where the original outcome data are
used as the outcome and the estimated class probabilities for one class
are used as the predictor. If `smooth = TRUE`, a generalized additive
model is fit using
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and the default
smoothing method. Otherwise, a simple logistic regression is used.

If the predictions are well calibrated, the fitted curve should align
with the diagonal line. Confidence intervals for the fitted line are
also shown.

## Usage

``` r
cal_plot_logistic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  conf_level = 0.9,
  smooth = TRUE,
  include_rug = TRUE,
  include_ribbon = TRUE,
  event_level = c("auto", "first", "second"),
  ...
)

# S3 method for class 'data.frame'
cal_plot_logistic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  conf_level = 0.9,
  smooth = TRUE,
  include_rug = TRUE,
  include_ribbon = TRUE,
  event_level = c("auto", "first", "second"),
  ...,
  .by = NULL
)

# S3 method for class 'tune_results'
cal_plot_logistic(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  conf_level = 0.9,
  smooth = TRUE,
  include_rug = TRUE,
  include_ribbon = TRUE,
  event_level = c("auto", "first", "second"),
  ...
)

# S3 method for class 'grouped_df'
cal_plot_logistic(
  .data,
  truth = NULL,
  estimate = NULL,
  conf_level = 0.9,
  smooth = TRUE,
  include_rug = TRUE,
  include_ribbon = TRUE,
  event_level = c("auto", "first", "second"),
  ...
)
```

## Arguments

- .data:

  An ungrouped data frame object containing predictions and probability
  columns.

- truth:

  The column identifier for the true class results (that is a factor).
  This should be an unquoted column name.

- estimate:

  A vector of column identifiers, or one of `dplyr` selector functions
  to choose which variables contains the class probabilities. It
  defaults to the prefix used by tidymodels (`.pred_`). The order of the
  identifiers will be considered the same as the order of the levels of
  the `truth` variable.

- conf_level:

  Confidence level to use in the visualization. It defaults to 0.9.

- smooth:

  A logical for using a generalized additive model with smooth terms for
  the predictor via
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html).

- include_rug:

  Flag that indicates if the Rug layer is to be included. It defaults to
  `TRUE`. In the plot, the top side shows the frequency the event
  occurring, and the bottom the frequency of the event not occurring.

- include_ribbon:

  Flag that indicates if the ribbon layer is to be included. It defaults
  to `TRUE`.

- event_level:

  single string. Either "first" or "second" to specify which level of
  truth to consider as the "event". Defaults to "auto", which allows the
  function decide which one to use based on the type of model (binary,
  multi-class or linear)

- ...:

  Additional arguments passed to the `tune_results` object.

- .by:

  The column identifier for the grouping variable. This should be a
  single unquoted column name that selects a qualitative variable for
  grouping. Default to `NULL`. When `.by = NULL` no grouping will take
  place.

## Value

A ggplot object.

## See also

<https://www.tidymodels.org/learn/models/calibration/>,
[`cal_plot_windowed()`](https://probably.tidymodels.org/dev/reference/cal_plot_windowed.md),
[`cal_plot_breaks()`](https://probably.tidymodels.org/dev/reference/cal_plot_breaks.md)

[`cal_plot_breaks()`](https://probably.tidymodels.org/dev/reference/cal_plot_breaks.md),
[`cal_plot_windowed()`](https://probably.tidymodels.org/dev/reference/cal_plot_windowed.md)

## Examples

``` r
library(ggplot2)
library(dplyr)

cal_plot_logistic(
  segment_logistic,
  Class,
  .pred_good
)


cal_plot_logistic(
  segment_logistic,
  Class,
  .pred_good,
  smooth = FALSE
)
```
