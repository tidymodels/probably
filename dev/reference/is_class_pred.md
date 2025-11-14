# Test if an object inherits from `class_pred`

`is_class_pred()` checks if an object is a `class_pred` object.

## Usage

``` r
is_class_pred(x)
```

## Arguments

- x:

  An object.

## Examples

``` r
x <- class_pred(factor(1:5))

is_class_pred(x)
#> [1] TRUE
```
