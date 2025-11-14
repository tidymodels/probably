# Extract `class_pred` levels

The levels of a `class_pred` object do *not* include the equivocal
value.

## Usage

``` r
# S3 method for class 'class_pred'
levels(x)
```

## Arguments

- x:

  A `class_pred` object.

## Examples

``` r
x <- class_pred(factor(1:5), which = 1)

# notice that even though `1` is not in the `class_pred` vector, the
# level remains from the original factor
levels(x)
#> [1] "1" "2" "3" "4" "5"
```
