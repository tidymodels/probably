# Coerce to a `class_pred` object

`as_class_pred()` provides coercion to `class_pred` from other existing
objects.

## Usage

``` r
as_class_pred(x, which = integer(), equivocal = "[EQ]")
```

## Arguments

- x:

  A factor or ordered factor.

- which:

  An integer vector specifying the locations of `x` to declare as
  equivocal.

- equivocal:

  A single character specifying the equivocal label used when printing.

## Examples

``` r
x <- factor(c("Yes", "No", "Yes", "Yes"))
as_class_pred(x)
#> [1] Yes No  Yes Yes
#> Levels: No Yes
#> Reportable: 100%
```
