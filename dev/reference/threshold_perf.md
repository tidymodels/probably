# Generate performance metrics across probability thresholds

`threshold_perf()` can take a set of class probability predictions and
determine performance characteristics across different values of the
probability threshold and any existing groups.

## Usage

``` r
threshold_perf(.data, ...)

# S3 method for class 'data.frame'
threshold_perf(
  .data,
  truth,
  estimate,
  thresholds = NULL,
  metrics = NULL,
  na_rm = TRUE,
  event_level = "first",
  ...
)
```

## Arguments

- .data:

  A tibble, potentially grouped.

- ...:

  Currently unused.

- truth:

  The column identifier for the true two-class results (that is a
  factor). This should be an unquoted column name.

- estimate:

  The column identifier for the predicted class probabilities (that is a
  numeric). This should be an unquoted column name.

- thresholds:

  A numeric vector of values for the probability threshold. If
  unspecified, a series of values between 0.5 and 1.0 are used.
  **Note**: if this argument is used, it must be named.

- metrics:

  Either `NULL` or a
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
  with a list of performance metrics to calculate. The metrics should
  all be oriented towards hard class predictions (e.g.
  [`yardstick::sensitivity()`](https://yardstick.tidymodels.org/reference/sens.html),
  [`yardstick::accuracy()`](https://yardstick.tidymodels.org/reference/accuracy.html),
  [`yardstick::recall()`](https://yardstick.tidymodels.org/reference/recall.html),
  etc.) and not class probabilities. A set of default metrics is used
  when `NULL` (see Details below).

- na_rm:

  A single logical: should missing data be removed?

- event_level:

  A single string. Either `"first"` or `"second"` to specify which level
  of `truth` to consider as the "event".

## Value

A tibble with columns: `.threshold`, `.estimator`, `.metric`,
`.estimate` and any existing groups.

## Details

Note that that the global option `yardstick.event_first` will be used to
determine which level is the event of interest. For more details, see
the Relevant level section of
[`yardstick::sens()`](https://yardstick.tidymodels.org/reference/sens.html).

The default calculated metrics are:

- [`yardstick::j_index()`](https://yardstick.tidymodels.org/reference/j_index.html)

- [`yardstick::sens()`](https://yardstick.tidymodels.org/reference/sens.html)

- [`yardstick::spec()`](https://yardstick.tidymodels.org/reference/spec.html)

- `distance = (1 - sens) ^ 2 + (1 - spec) ^ 2`

If a custom metric is passed that does not compute sensitivity and
specificity, the distance metric is not computed.

## Examples

``` r
library(dplyr)
data("segment_logistic")

# Set the threshold to 0.6
# > 0.6 = good
# < 0.6 = poor
threshold_perf(segment_logistic, Class, .pred_good, thresholds = 0.6)
#> # A tibble: 4 × 4
#>   .threshold .metric     .estimator .estimate
#>        <dbl> <chr>       <chr>          <dbl>
#> 1        0.6 sensitivity binary         0.639
#> 2        0.6 specificity binary         0.869
#> 3        0.6 j_index     binary         0.508
#> 4        0.6 distance    binary         0.148

# Set the threshold to multiple values
thresholds <- seq(0.5, 0.9, by = 0.1)

segment_logistic |>
  threshold_perf(Class, .pred_good, thresholds)
#> # A tibble: 20 × 4
#>    .threshold .metric     .estimator .estimate
#>         <dbl> <chr>       <chr>          <dbl>
#>  1        0.5 sensitivity binary         0.714
#>  2        0.6 sensitivity binary         0.639
#>  3        0.7 sensitivity binary         0.561
#>  4        0.8 sensitivity binary         0.451
#>  5        0.9 sensitivity binary         0.249
#>  6        0.5 specificity binary         0.825
#>  7        0.6 specificity binary         0.869
#>  8        0.7 specificity binary         0.911
#>  9        0.8 specificity binary         0.937
#> 10        0.9 specificity binary         0.977
#> 11        0.5 j_index     binary         0.539
#> 12        0.6 j_index     binary         0.508
#> 13        0.7 j_index     binary         0.472
#> 14        0.8 j_index     binary         0.388
#> 15        0.9 j_index     binary         0.226
#> 16        0.5 distance    binary         0.112
#> 17        0.6 distance    binary         0.148
#> 18        0.7 distance    binary         0.201
#> 19        0.8 distance    binary         0.306
#> 20        0.9 distance    binary         0.565

# ---------------------------------------------------------------------------

# It works with grouped data frames as well
# Let's mock some resampled data
resamples <- 5

mock_resamples <- resamples |>
  replicate(
    expr = sample_n(segment_logistic, 100, replace = TRUE),
    simplify = FALSE
  ) |>
  bind_rows(.id = "resample")

resampled_threshold_perf <- mock_resamples |>
  group_by(resample) |>
  threshold_perf(Class, .pred_good, thresholds)

resampled_threshold_perf
#> # A tibble: 100 × 5
#>    resample .threshold .metric     .estimator .estimate
#>    <chr>         <dbl> <chr>       <chr>          <dbl>
#>  1 1               0.5 sensitivity binary         0.711
#>  2 1               0.6 sensitivity binary         0.632
#>  3 1               0.7 sensitivity binary         0.579
#>  4 1               0.8 sensitivity binary         0.421
#>  5 1               0.9 sensitivity binary         0.316
#>  6 2               0.5 sensitivity binary         0.781
#>  7 2               0.6 sensitivity binary         0.625
#>  8 2               0.7 sensitivity binary         0.594
#>  9 2               0.8 sensitivity binary         0.469
#> 10 2               0.9 sensitivity binary         0.188
#> # ℹ 90 more rows

# Average over the resamples
resampled_threshold_perf |>
  group_by(.metric, .threshold) |>
  summarise(.estimate = mean(.estimate))
#> `summarise()` has grouped output by '.metric'. You can override using
#> the `.groups` argument.
#> # A tibble: 20 × 3
#> # Groups:   .metric [4]
#>    .metric     .threshold .estimate
#>    <chr>            <dbl>     <dbl>
#>  1 distance           0.5     0.140
#>  2 distance           0.6     0.171
#>  3 distance           0.7     0.212
#>  4 distance           0.8     0.335
#>  5 distance           0.9     0.590
#>  6 j_index            0.5     0.494
#>  7 j_index            0.6     0.473
#>  8 j_index            0.7     0.466
#>  9 j_index            0.8     0.374
#> 10 j_index            0.9     0.213
#> 11 sensitivity        0.5     0.698
#> 12 sensitivity        0.6     0.621
#> 13 sensitivity        0.7     0.553
#> 14 sensitivity        0.8     0.428
#> 15 sensitivity        0.9     0.235
#> 16 specificity        0.5     0.796
#> 17 specificity        0.6     0.852
#> 18 specificity        0.7     0.913
#> 19 specificity        0.8     0.946
#> 20 specificity        0.9     0.978
```
