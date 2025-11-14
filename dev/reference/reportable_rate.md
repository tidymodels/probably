# Calculate the reportable rate

The *reportable rate* is defined as the percentage of class predictions
that are *not* equivocal.

## Usage

``` r
reportable_rate(x)
```

## Arguments

- x:

  A `class_pred` object.

## Details

The reportable rate is calculated as `(n_not_equivocal / n)`.

## Examples

``` r
x <- class_pred(factor(1:5), which = c(1, 2))

# 3 / 5
reportable_rate(x)
#> [1] 0.6
```
