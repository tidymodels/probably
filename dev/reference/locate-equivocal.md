# Locate equivocal values

These functions provide multiple methods of checking for equivocal
values, and finding their locations.

## Usage

``` r
is_equivocal(x)

which_equivocal(x)

any_equivocal(x)
```

## Arguments

- x:

  A `class_pred` object.

## Value

`is_equivocal()` returns a logical vector the same length as `x` where
`TRUE` means the value is equivocal.

`which_equivocal()` returns an integer vector specifying the locations
of the equivocal values.

`any_equivocal()` returns `TRUE` if there are any equivocal values.

## Examples

``` r
x <- class_pred(factor(1:10), which = c(2, 5))

is_equivocal(x)
#>  [1] FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE

which_equivocal(x)
#> [1] 2 5

any_equivocal(x)
#> [1] TRUE
```
