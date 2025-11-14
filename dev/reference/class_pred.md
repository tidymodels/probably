# Create a class prediction object

`class_pred()` creates a `class_pred` object from a factor or ordered
factor. You can optionally specify values of the factor to be set as
*equivocal*.

## Usage

``` r
class_pred(x = factor(), which = integer(), equivocal = "[EQ]")
```

## Arguments

- x:

  A factor or ordered factor.

- which:

  An integer vector specifying the locations of `x` to declare as
  equivocal.

- equivocal:

  A single character specifying the equivocal label used when printing.

## Details

Equivocal values are those that you feel unsure about, and would like to
exclude from performance calculations or other metrics.

## Examples

``` r
x <- factor(c("Yes", "No", "Yes", "Yes"))

# Create a class_pred object from a factor
class_pred(x)
#> [1] Yes No  Yes Yes
#> Levels: No Yes
#> Reportable: 100%

# Say you aren't sure about that 2nd "Yes" value. You could mark it as
# equivocal.
class_pred(x, which = 3)
#> [1] Yes  No   [EQ] Yes 
#> Levels: No Yes
#> Reportable: 75%

# Maybe you want a different equivocal label
class_pred(x, which = 3, equivocal = "eq_value")
#> [1] Yes      No       eq_value Yes     
#> Levels: No Yes
#> Reportable: 75%
```
