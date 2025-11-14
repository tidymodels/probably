# Obtain and format metrics produced by calibration validation

Obtain and format metrics produced by calibration validation

## Usage

``` r
# S3 method for class 'cal_rset'
collect_metrics(x, summarize = TRUE, ...)
```

## Arguments

- x:

  An object produced by one of the validation function (or class
  `cal_rset`).

- summarize:

  A logical; should metrics be summarized over resamples (`TRUE`) or
  return the values for each individual resample. See
  [`tune::collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  for more details.

- ...:

  Not currently used.

## Value

A tibble
