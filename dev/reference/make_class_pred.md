# Create a `class_pred` vector from class probabilities

These functions can be used to convert class probability estimates to
`class_pred` objects with an optional equivocal zone.

## Usage

``` r
make_class_pred(..., levels, ordered = FALSE, min_prob = 1/length(levels))

make_two_class_pred(
  estimate,
  levels,
  threshold = 0.5,
  ordered = FALSE,
  buffer = NULL
)
```

## Arguments

- ...:

  Numeric vectors corresponding to class probabilities. There should be
  one for each level in `levels`, and *it is assumed that the vectors
  are in the same order as `levels`*.

- levels:

  A character vector of class levels. The length should be the same as
  the number of selections made through `...`, or length `2` for
  `make_two_class_pred()`.

- ordered:

  A single logical to determine if the levels should be regarded as
  ordered (in the order given). This results in a `class_pred` object
  that is flagged as ordered.

- min_prob:

  A single numeric value. If any probabilities are less than this value
  (by row), the row is marked as *equivocal*.

- estimate:

  A single numeric vector corresponding to the class probabilities of
  the first level in `levels`.

- threshold:

  A single numeric value for the threshold to call a row to be labeled
  as the first value of `levels`.

- buffer:

  A numeric vector of length 1 or 2 for the buffer around `threshold`
  that defines the equivocal zone (i.e., `threshold - buffer[1]` to
  `threshold + buffer[2]`). A length 1 vector is recycled to length 2.
  The default, `NULL`, is interpreted as no equivocal zone.

## Value

A vector of class
[`class_pred`](https://probably.tidymodels.org/dev/reference/class_pred.md).

## Examples

``` r
library(dplyr)

good <- segment_logistic$.pred_good
lvls <- levels(segment_logistic$Class)

# Equivocal zone of .5 +/- .15
make_two_class_pred(good, lvls, buffer = 0.15)
#>    [1] poor poor good good poor [EQ] poor good poor poor good poor poor
#>   [14] good poor good poor poor [EQ] poor poor good poor good poor poor
#>   [27] good [EQ] good good poor poor [EQ] good good poor poor poor poor
#>   [40] poor [EQ] [EQ] poor poor poor poor poor good good poor [EQ] poor
#>   [53] poor [EQ] poor [EQ] poor poor [EQ] poor poor good good poor poor
#>   [66] [EQ] good poor [EQ] poor good poor [EQ] poor good poor good [EQ]
#>   [79] poor [EQ] good poor poor poor good good poor good poor poor poor
#>   [92] poor [EQ] good poor [EQ] good [EQ] [EQ] poor good [EQ] poor poor
#>  [105] good good good [EQ] good poor poor poor good poor [EQ] poor [EQ]
#>  [118] poor good good poor good poor [EQ] poor poor good good poor poor
#>  [131] [EQ] [EQ] poor good good poor [EQ] poor [EQ] [EQ] poor [EQ] [EQ]
#>  [144] poor [EQ] poor good [EQ] poor poor poor good good poor poor [EQ]
#>  [157] good poor poor [EQ] good poor good good poor poor poor [EQ] [EQ]
#>  [170] poor good good poor poor [EQ] good poor poor poor poor [EQ] good
#>  [183] poor poor poor [EQ] poor good good poor good [EQ] poor good poor
#>  [196] poor good poor poor poor good good [EQ] poor poor poor poor poor
#>  [209] [EQ] poor good good poor [EQ] poor poor poor good poor good [EQ]
#>  [222] poor good good good poor [EQ] poor poor good poor poor poor good
#>  [235] good good poor poor poor [EQ] [EQ] poor poor poor [EQ] poor [EQ]
#>  [248] good poor poor poor good poor poor good [EQ] good good poor [EQ]
#>  [261] poor good good [EQ] [EQ] good poor poor poor poor [EQ] poor poor
#>  [274] poor poor [EQ] good poor [EQ] [EQ] poor poor poor good [EQ] poor
#>  [287] [EQ] good poor [EQ] poor good [EQ] good poor poor poor good poor
#>  [300] good [EQ] poor poor poor good poor poor poor [EQ] good poor poor
#>  [313] good poor good poor [EQ] poor poor [EQ] [EQ] poor poor poor poor
#>  [326] good [EQ] poor poor [EQ] poor poor poor poor poor [EQ] good [EQ]
#>  [339] poor good poor good [EQ] good poor [EQ] poor poor poor poor poor
#>  [352] good good [EQ] [EQ] poor good poor good poor poor poor poor good
#>  [365] poor poor poor poor poor poor poor [EQ] poor poor poor poor poor
#>  [378] poor poor good poor poor poor poor poor poor [EQ] good poor poor
#>  [391] poor [EQ] [EQ] good poor poor poor poor poor poor good [EQ] [EQ]
#>  [404] poor poor poor poor poor poor poor good good poor poor poor poor
#>  [417] poor [EQ] poor poor poor good [EQ] good good poor poor poor good
#>  [430] good good good poor good poor poor poor poor poor poor good [EQ]
#>  [443] [EQ] poor good good [EQ] [EQ] poor poor good poor poor good poor
#>  [456] good poor poor poor good poor poor poor poor good poor poor good
#>  [469] poor good good good poor good poor good good good poor poor good
#>  [482] poor poor poor poor poor poor good [EQ] poor [EQ] poor poor poor
#>  [495] good poor [EQ] poor [EQ] poor poor poor poor poor poor good good
#>  [508] poor [EQ] [EQ] [EQ] poor poor poor poor good [EQ] good poor poor
#>  [521] good good poor [EQ] poor poor [EQ] poor good poor poor good poor
#>  [534] poor poor poor poor good [EQ] poor good good poor poor good poor
#>  [547] good good poor poor good poor good poor [EQ] poor poor poor poor
#>  [560] [EQ] good poor good good poor poor poor good good poor poor good
#>  [573] [EQ] [EQ] poor [EQ] poor poor poor [EQ] poor good poor good good
#>  [586] poor poor poor poor good [EQ] good poor good [EQ] [EQ] poor poor
#>  [599] [EQ] [EQ] poor good good good [EQ] good poor poor poor [EQ] poor
#>  [612] good poor good [EQ] poor poor poor good good poor good poor poor
#>  [625] poor poor poor good poor [EQ] good [EQ] good poor good poor good
#>  [638] poor poor [EQ] [EQ] poor poor poor poor poor good good poor poor
#>  [651] poor poor poor poor good good poor good poor good poor good poor
#>  [664] poor poor [EQ] poor poor good poor poor good good good poor poor
#>  [677] poor [EQ] poor good good [EQ] good poor good poor poor poor [EQ]
#>  [690] poor poor [EQ] [EQ] good [EQ] poor good poor poor good good poor
#>  [703] [EQ] poor good poor poor [EQ] [EQ] [EQ] poor good poor good good
#>  [716] good good poor poor poor good poor good poor poor [EQ] poor poor
#>  [729] poor poor poor poor [EQ] good good good poor [EQ] poor poor poor
#>  [742] good poor good good [EQ] poor good poor [EQ] poor poor poor [EQ]
#>  [755] good good poor poor poor good poor good poor good [EQ] poor good
#>  [768] [EQ] poor [EQ] good poor good [EQ] poor good poor poor good poor
#>  [781] poor good good good poor poor poor poor poor good poor [EQ] poor
#>  [794] poor poor good [EQ] poor good [EQ] [EQ] good poor good poor poor
#>  [807] poor poor poor poor poor [EQ] poor good poor poor poor poor good
#>  [820] poor good good poor poor poor poor poor good [EQ] poor good poor
#>  [833] poor poor poor poor poor poor [EQ] poor poor poor poor poor good
#>  [846] good good poor poor poor poor poor poor poor poor good [EQ] [EQ]
#>  [859] [EQ] poor good [EQ] poor poor poor [EQ] good poor good good poor
#>  [872] good poor poor good [EQ] [EQ] [EQ] poor poor poor poor [EQ] good
#>  [885] poor good poor good poor poor poor poor good poor poor poor poor
#>  [898] poor poor poor poor [EQ] poor poor [EQ] good [EQ] good poor poor
#>  [911] poor good [EQ] poor good poor poor poor poor good poor poor good
#>  [924] good poor poor good poor [EQ] poor good poor good good good poor
#>  [937] poor good good poor poor [EQ] [EQ] poor good poor poor good [EQ]
#>  [950] [EQ] poor [EQ] poor good [EQ] [EQ] poor good poor poor poor good
#>  [963] poor poor poor poor good poor poor [EQ] poor poor poor good good
#>  [976] poor [EQ] poor poor poor good poor poor good poor [EQ] good good
#>  [989] good good poor [EQ] poor good poor poor poor poor good good good
#> [1002] good good poor good [EQ] good poor poor good
#> Levels: good poor
#> Reportable: 83.2%

# Equivocal zone of c(.5 - .05, .5 + .15)
make_two_class_pred(good, lvls, buffer = c(0.05, 0.15))
#>    [1] poor poor good good poor poor poor good poor poor good poor poor
#>   [14] good poor good poor poor [EQ] poor poor good poor good poor poor
#>   [27] good poor good good poor poor [EQ] good good poor poor poor poor
#>   [40] poor [EQ] poor poor poor poor poor poor good good poor [EQ] poor
#>   [53] poor poor poor poor poor poor [EQ] poor poor good good poor poor
#>   [66] poor good poor [EQ] poor good poor [EQ] poor good poor good poor
#>   [79] poor [EQ] good poor poor poor good good poor good poor poor poor
#>   [92] poor [EQ] good poor [EQ] good [EQ] poor poor good [EQ] poor poor
#>  [105] good good good poor good poor poor poor good poor poor poor [EQ]
#>  [118] poor good good poor good poor [EQ] poor poor good good poor poor
#>  [131] [EQ] poor poor good good poor [EQ] poor [EQ] [EQ] poor poor [EQ]
#>  [144] poor [EQ] poor good poor poor poor poor good good poor poor poor
#>  [157] good poor poor [EQ] good poor good good poor poor poor [EQ] poor
#>  [170] poor good good poor poor [EQ] good poor poor poor poor [EQ] good
#>  [183] poor poor poor [EQ] poor good good poor good poor poor good poor
#>  [196] poor good poor poor poor good good poor poor poor poor poor poor
#>  [209] poor poor good good poor [EQ] poor poor poor good poor good [EQ]
#>  [222] poor good good good poor [EQ] poor poor good poor poor poor good
#>  [235] good good poor poor poor [EQ] poor poor poor poor [EQ] poor poor
#>  [248] good poor poor poor good poor poor good [EQ] good good poor [EQ]
#>  [261] poor good good [EQ] poor good poor poor poor poor [EQ] poor poor
#>  [274] poor poor [EQ] good poor poor [EQ] poor poor poor good [EQ] poor
#>  [287] [EQ] good poor poor poor good [EQ] good poor poor poor good poor
#>  [300] good [EQ] poor poor poor good poor poor poor [EQ] good poor poor
#>  [313] good poor good poor poor poor poor [EQ] [EQ] poor poor poor poor
#>  [326] good poor poor poor [EQ] poor poor poor poor poor [EQ] good [EQ]
#>  [339] poor good poor good poor good poor [EQ] poor poor poor poor poor
#>  [352] good good poor [EQ] poor good poor good poor poor poor poor good
#>  [365] poor poor poor poor poor poor poor poor poor poor poor poor poor
#>  [378] poor poor good poor poor poor poor poor poor [EQ] good poor poor
#>  [391] poor [EQ] [EQ] good poor poor poor poor poor poor good [EQ] [EQ]
#>  [404] poor poor poor poor poor poor poor good good poor poor poor poor
#>  [417] poor [EQ] poor poor poor good [EQ] good good poor poor poor good
#>  [430] good good good poor good poor poor poor poor poor poor good [EQ]
#>  [443] poor poor good good [EQ] [EQ] poor poor good poor poor good poor
#>  [456] good poor poor poor good poor poor poor poor good poor poor good
#>  [469] poor good good good poor good poor good good good poor poor good
#>  [482] poor poor poor poor poor poor good poor poor [EQ] poor poor poor
#>  [495] good poor poor poor [EQ] poor poor poor poor poor poor good good
#>  [508] poor [EQ] [EQ] [EQ] poor poor poor poor good poor good poor poor
#>  [521] good good poor poor poor poor [EQ] poor good poor poor good poor
#>  [534] poor poor poor poor good poor poor good good poor poor good poor
#>  [547] good good poor poor good poor good poor poor poor poor poor poor
#>  [560] poor good poor good good poor poor poor good good poor poor good
#>  [573] [EQ] poor poor poor poor poor poor poor poor good poor good good
#>  [586] poor poor poor poor good [EQ] good poor good poor [EQ] poor poor
#>  [599] poor [EQ] poor good good good [EQ] good poor poor poor poor poor
#>  [612] good poor good poor poor poor poor good good poor good poor poor
#>  [625] poor poor poor good poor poor good poor good poor good poor good
#>  [638] poor poor [EQ] [EQ] poor poor poor poor poor good good poor poor
#>  [651] poor poor poor poor good good poor good poor good poor good poor
#>  [664] poor poor [EQ] poor poor good poor poor good good good poor poor
#>  [677] poor [EQ] poor good good [EQ] good poor good poor poor poor poor
#>  [690] poor poor poor [EQ] good [EQ] poor good poor poor good good poor
#>  [703] poor poor good poor poor [EQ] [EQ] poor poor good poor good good
#>  [716] good good poor poor poor good poor good poor poor [EQ] poor poor
#>  [729] poor poor poor poor [EQ] good good good poor poor poor poor poor
#>  [742] good poor good good poor poor good poor [EQ] poor poor poor [EQ]
#>  [755] good good poor poor poor good poor good poor good [EQ] poor good
#>  [768] [EQ] poor [EQ] good poor good [EQ] poor good poor poor good poor
#>  [781] poor good good good poor poor poor poor poor good poor [EQ] poor
#>  [794] poor poor good [EQ] poor good poor poor good poor good poor poor
#>  [807] poor poor poor poor poor poor poor good poor poor poor poor good
#>  [820] poor good good poor poor poor poor poor good [EQ] poor good poor
#>  [833] poor poor poor poor poor poor poor poor poor poor poor poor good
#>  [846] good good poor poor poor poor poor poor poor poor good [EQ] poor
#>  [859] [EQ] poor good poor poor poor poor [EQ] good poor good good poor
#>  [872] good poor poor good [EQ] poor [EQ] poor poor poor poor [EQ] good
#>  [885] poor good poor good poor poor poor poor good poor poor poor poor
#>  [898] poor poor poor poor poor poor poor poor good poor good poor poor
#>  [911] poor good [EQ] poor good poor poor poor poor good poor poor good
#>  [924] good poor poor good poor [EQ] poor good poor good good good poor
#>  [937] poor good good poor poor poor poor poor good poor poor good [EQ]
#>  [950] poor poor [EQ] poor good [EQ] poor poor good poor poor poor good
#>  [963] poor poor poor poor good poor poor [EQ] poor poor poor good good
#>  [976] poor poor poor poor poor good poor poor good poor poor good good
#>  [989] good good poor [EQ] poor good poor poor poor poor good good good
#> [1002] good good poor good [EQ] good poor poor good
#> Levels: good poor
#> Reportable: 89.8%

# These functions are useful alongside dplyr::mutate()
segment_logistic |>
  mutate(
    .class_pred = make_two_class_pred(
      estimate = .pred_good,
      levels = levels(Class),
      buffer = 0.15
    )
  )
#> # A tibble: 1,010 × 4
#>    .pred_poor .pred_good Class .class_pred
#>         <dbl>      <dbl> <fct>  <clss_prd>
#>  1    0.986      0.0142  poor         poor
#>  2    0.897      0.103   poor         poor
#>  3    0.118      0.882   good         good
#>  4    0.102      0.898   good         good
#>  5    0.991      0.00914 poor         poor
#>  6    0.633      0.367   good         [EQ]
#>  7    0.770      0.230   good         poor
#>  8    0.00842    0.992   good         good
#>  9    0.995      0.00458 poor         poor
#> 10    0.765      0.235   poor         poor
#> # ℹ 1,000 more rows

# Multi-class example
# Note that we provide class probability columns in the same
# order as the levels
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
```
