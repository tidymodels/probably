# Probability Calibration table

Calibration table functions. They require a data.frame that contains the
predictions and probability columns. The output is another `tibble` with
segmented data that compares the accuracy of the probability to the
actual outcome.

## Usage

``` r
.cal_table_breaks(
  .data,
  truth = NULL,
  estimate = NULL,
  .by = NULL,
  num_breaks = 10,
  conf_level = 0.9,
  event_level = c("auto", "first", "second"),
  ...
)

.cal_table_logistic(
  .data,
  truth = NULL,
  estimate = NULL,
  .by = NULL,
  conf_level = 0.9,
  smooth = TRUE,
  event_level = c("auto", "first", "second"),
  ...
)

.cal_table_windowed(
  .data,
  truth = NULL,
  estimate = NULL,
  .by = NULL,
  window_size = 0.1,
  step_size = window_size/2,
  conf_level = 0.9,
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

- .by:

  The column identifier for the grouping variable. This should be a
  single unquoted column name that selects a qualitative variable for
  grouping. Default to `NULL`. When `.by = NULL` no grouping will take
  place.

- num_breaks:

  The number of segments to group the probabilities. It defaults to 10.

- conf_level:

  Confidence level to use in the visualization. It defaults to 0.9.

- event_level:

  single string. Either "first" or "second" to specify which level of
  truth to consider as the "event". Defaults to "auto", which allows the
  function decide which one to use based on the type of model (binary,
  multi-class or linear)

- ...:

  Additional arguments passed to the `tune_results` object.

## Details

- `.cal_table_breaks()` - Splits the data into bins, based on the number
  of breaks provided (`num_breaks`). The bins are even ranges, starting
  at 0, and ending at 1.

- `.cal_table_logistic()` - Fits a logistic spline regression (GAM)
  against the data. It then creates a table with the predictions based
  on 100 probabilities starting at 0, and ending at 1.

- `.cal_table_windowed()` - Creates a running percentage of the
  probability that moves across the proportion of events.

## Examples

``` r
.cal_table_breaks(
  segment_logistic,
  Class,
  .pred_good
)
#> # A tibble: 10 × 6
#>    predicted_midpoint event_rate events total  lower  upper
#>                 <dbl>      <dbl>  <dbl> <int>  <dbl>  <dbl>
#>  1               0.05     0.0350     12   343 0.0208 0.0570
#>  2               0.15     0.0841      9   107 0.0461 0.145 
#>  3               0.25     0.324      24    74 0.236  0.426 
#>  4               0.35     0.366      26    71 0.272  0.471 
#>  5               0.45     0.538      28    52 0.416  0.656 
#>  6               0.55     0.473      26    55 0.357  0.591 
#>  7               0.65     0.491      27    55 0.374  0.608 
#>  8               0.75     0.691      38    55 0.572  0.790 
#>  9               0.85     0.722      70    97 0.636  0.794 
#> 10               0.95     0.851      86   101 0.779  0.905 

.cal_table_logistic(
  segment_logistic,
  Class,
  .pred_good
)
#> # A tibble: 101 × 4
#>    estimate   prob  lower  upper
#>       <dbl>  <dbl>  <dbl>  <dbl>
#>  1     0    0.0219 0.0143 0.0335
#>  2     0.01 0.0246 0.0165 0.0365
#>  3     0.02 0.0276 0.0190 0.0399
#>  4     0.03 0.0310 0.0219 0.0437
#>  5     0.04 0.0347 0.0250 0.0479
#>  6     0.05 0.0389 0.0286 0.0527
#>  7     0.06 0.0435 0.0325 0.0580
#>  8     0.07 0.0487 0.0369 0.0640
#>  9     0.08 0.0544 0.0418 0.0706
#> 10     0.09 0.0608 0.0472 0.0780
#> # ℹ 91 more rows

.cal_table_windowed(
  segment_logistic,
  Class,
  .pred_good
)
#> # A tibble: 21 × 6
#>    predicted_midpoint event_rate events total  lower  upper
#>                 <dbl>      <dbl>  <dbl> <int>  <dbl>  <dbl>
#>  1              0.025     0.0233      6   258 0.0108 0.0468
#>  2              0.05      0.0350     12   343 0.0208 0.0570
#>  3              0.1       0.0559      8   143 0.0293 0.101 
#>  4              0.15      0.0841      9   107 0.0461 0.145 
#>  5              0.2       0.195      17    87 0.130  0.280 
#>  6              0.25      0.324      24    74 0.236  0.426 
#>  7              0.3       0.343      24    70 0.251  0.448 
#>  8              0.35      0.366      26    71 0.272  0.471 
#>  9              0.4       0.433      29    67 0.331  0.540 
#> 10              0.45      0.538      28    52 0.416  0.656 
#> # ℹ 11 more rows
```
