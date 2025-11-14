# Obtain and format predictions produced by calibration validation

Obtain and format predictions produced by calibration validation

## Usage

``` r
# S3 method for class 'cal_rset'
collect_predictions(x, summarize = TRUE, ...)
```

## Arguments

- x:

  An object produced by one of the validation function (or class
  `cal_rset`).

- summarize:

  A logical; should predictions be summarized over resamples (`TRUE`) or
  return the values for each individual resample. See
  [`tune::collect_predictions()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  for more details.

- ...:

  Not currently used.

## Value

A tibble
