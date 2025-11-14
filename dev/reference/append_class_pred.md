# Add a `class_pred` column

This function is similar to
[`make_class_pred()`](https://probably.tidymodels.org/dev/reference/make_class_pred.md),
but is useful when you have a large number of class probability columns
and want to use `tidyselect` helpers. It appends the new `class_pred`
vector as a column on the original data frame.

## Usage

``` r
append_class_pred(
  .data,
  ...,
  levels,
  ordered = FALSE,
  min_prob = 1/length(levels),
  name = ".class_pred"
)
```

## Arguments

- .data:

  A data frame or tibble.

- ...:

  One or more unquoted expressions separated by commas to capture the
  columns of `.data` containing the class probabilities. You can treat
  variable names like they are positions, so you can use expressions
  like `x:y` to select ranges of variables or use selector functions to
  choose which columns. For `make_class_pred`, the columns for all class
  probabilities should be selected (in the same order as the `levels`
  object). For `two_class_pred`, a vector of class probabilities should
  be selected.

- levels:

  A character vector of class levels. The length should be the same as
  the number of selections made through `...`, or length `2` for
  [`make_two_class_pred()`](https://probably.tidymodels.org/dev/reference/make_class_pred.md).

- ordered:

  A single logical to determine if the levels should be regarded as
  ordered (in the order given). This results in a `class_pred` object
  that is flagged as ordered.

- min_prob:

  A single numeric value. If any probabilities are less than this value
  (by row), the row is marked as *equivocal*.

- name:

  A single character value for the name of the appended `class_pred`
  column.

## Value

`.data` with an extra `class_pred` column appended onto it.

## Examples

``` r
# The following two examples are equivalent and demonstrate
# the helper, append_class_pred()

library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

species_probs |>
  mutate(
    .class_pred = make_class_pred(
      .pred_bobcat, .pred_coyote, .pred_gray_fox,
      levels = levels(Species),
      min_prob = .5
    )
  )
#> # A tibble: 110 × 5
#>    Species  .pred_bobcat .pred_coyote .pred_gray_fox .class_pred
#>    <fct>           <dbl>        <dbl>          <dbl>  <clss_prd>
#>  1 gray_fox       0.0976       0.0530         0.849     gray_fox
#>  2 gray_fox       0.155        0.139          0.706     gray_fox
#>  3 bobcat         0.501        0.0880         0.411       bobcat
#>  4 gray_fox       0.256        0              0.744     gray_fox
#>  5 gray_fox       0.463        0.287          0.250         [EQ]
#>  6 bobcat         0.811        0              0.189       bobcat
#>  7 bobcat         0.911        0.0888         0           bobcat
#>  8 bobcat         0.898        0.0517         0.0500      bobcat
#>  9 bobcat         0.771        0.229          0           bobcat
#> 10 bobcat         0.623        0.325          0.0517      bobcat
#> # ℹ 100 more rows

lvls <- levels(species_probs$Species)

append_class_pred(
  .data = species_probs,
  contains(".pred_"),
  levels = lvls,
  min_prob = .5
)
#> # A tibble: 110 × 5
#>    Species  .pred_bobcat .pred_coyote .pred_gray_fox .class_pred
#>    <fct>           <dbl>        <dbl>          <dbl>  <clss_prd>
#>  1 gray_fox       0.0976       0.0530         0.849     gray_fox
#>  2 gray_fox       0.155        0.139          0.706     gray_fox
#>  3 bobcat         0.501        0.0880         0.411       bobcat
#>  4 gray_fox       0.256        0              0.744     gray_fox
#>  5 gray_fox       0.463        0.287          0.250         [EQ]
#>  6 bobcat         0.811        0              0.189       bobcat
#>  7 bobcat         0.911        0.0888         0           bobcat
#>  8 bobcat         0.898        0.0517         0.0500      bobcat
#>  9 bobcat         0.771        0.229          0           bobcat
#> 10 bobcat         0.623        0.325          0.0517      bobcat
#> # ℹ 100 more rows
```
