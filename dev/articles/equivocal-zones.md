# Equivocal zones

``` r
library(probably)
```

## Equivocal zones

In some fields, class probability predictions must meet certain
standards before a firm decision can be made using them. If they fail
these standards, the prediction can be marked as *equivocal*, which just
means that you are unsure of the true result. You might want to further
investigate these equivocal values, or rerun whatever process generated
them before proceeding.

For example, in a binary model, if a prediction returned probability
values of 52% Yes and 48% No, are you really sure that that isn’t just
random noise? In this case, you could use a *buffer* surrounding a
*threshold* of 50% to determine whether or not your model is sure of its
predictions, and mark values you are unsure about as equivocal.

Another example could come from a Bayesian perspective, where each
prediction comes with a probability distribution. Your model might
predict 80% Yes, but could have a standard deviation around that of +/-
20%. In this case, you could set a maximum allowed standard deviation as
the cutoff of whether or not to mark values as equivocal.

To work with these equivocal zones, probably provides a new class for
hard class predictions that is very similar to a factor, but allows you
to mark certain values as equivocal.

``` r
x <- factor(c("Yes", "No", "Yes", "Yes"))

# Create a class_pred object from a factor
class_pred(x)
#> [1] Yes No  Yes Yes
#> Levels: No Yes
#> Reportable: 100%

# Say you aren't sure about that 2nd "Yes" value.
# You could mark it as equivocal.
class_pred(x, which = 3)
#> [1] Yes  No   [EQ] Yes 
#> Levels: No Yes
#> Reportable: 75%
```

The *reportable rate* is the fraction of values that are not equivocal,
relative to the total number. Above, you can see that the reportable
rate started at 100%, but as soon as a single value was marked
equivocal, that value dropped to 75%. In fields where equivocal zones
are used, there is often a tradeoff between marking values as equivocal
and keeping a certain minimum reportable rate.

Generally, you won’t create these `class_pred` objects directly, but
will instead create them indirectly through converting class
probabilities into class predictions with
[`make_class_pred()`](https://probably.tidymodels.org/dev/reference/make_class_pred.md)
and
[`make_two_class_pred()`](https://probably.tidymodels.org/dev/reference/make_class_pred.md).

``` r
library(dplyr)
data("segment_logistic")
segment_logistic
#> # A tibble: 1,010 × 3
#>    .pred_poor .pred_good Class
#>  *      <dbl>      <dbl> <fct>
#>  1    0.986      0.0142  poor 
#>  2    0.897      0.103   poor 
#>  3    0.118      0.882   good 
#>  4    0.102      0.898   good 
#>  5    0.991      0.00914 poor 
#>  6    0.633      0.367   good 
#>  7    0.770      0.230   good 
#>  8    0.00842    0.992   good 
#>  9    0.995      0.00458 poor 
#> 10    0.765      0.235   poor 
#> # ℹ 1,000 more rows

# Convert probabilities into predictions
# > 0.5 = good
# < 0.5 = poor
segment_logistic_thresh <- segment_logistic |>
  mutate(
    .pred = make_two_class_pred(
      estimate = .pred_good,
      levels = levels(Class),
      threshold = 0.5
    )
  )

segment_logistic_thresh
#> # A tibble: 1,010 × 4
#>    .pred_poor .pred_good Class      .pred
#>         <dbl>      <dbl> <fct> <clss_prd>
#>  1    0.986      0.0142  poor        poor
#>  2    0.897      0.103   poor        poor
#>  3    0.118      0.882   good        good
#>  4    0.102      0.898   good        good
#>  5    0.991      0.00914 poor        poor
#>  6    0.633      0.367   good        poor
#>  7    0.770      0.230   good        poor
#>  8    0.00842    0.992   good        good
#>  9    0.995      0.00458 poor        poor
#> 10    0.765      0.235   poor        poor
#> # ℹ 1,000 more rows
```

If a `buffer` is used, an equivocal zone is created around the threshold
of `threshold +/- buffer` and any values inside the zone are
automatically marked as equivocal.

``` r
# Convert probabilities into predictions
#        x > 0.55 = good
#        x < 0.45 = poor
# 0.45 < x < 0.55 = equivocal
segment_pred <- segment_logistic |>
  mutate(
    .pred = make_two_class_pred(
      estimate = .pred_good,
      levels = levels(Class),
      threshold = 0.5,
      buffer = 0.05
    )
  )

segment_pred |>
  count(.pred)
#> # A tibble: 3 × 2
#>        .pred     n
#>   <clss_prd> <int>
#> 1       [EQ]    45
#> 2       good   340
#> 3       poor   625

segment_pred |>
  summarise(reportable = reportable_rate(.pred))
#> # A tibble: 1 × 1
#>   reportable
#>        <dbl>
#> 1      0.955
```

Equivocal values in `class_pred` objects are converted to `NA` when the
object is converted to a factor. It’s also worth noting that the `[EQ]`
label is not treated as a separate level.

``` r
segment_pred |>
  mutate(.pred_fct = as.factor(.pred)) |>
  count(.pred, .pred_fct)
#> # A tibble: 3 × 3
#>        .pred .pred_fct     n
#>   <clss_prd> <fct>     <int>
#> 1       [EQ] NA           45
#> 2       good good        340
#> 3       poor poor        625

levels(segment_pred$.pred)
#> [1] "good" "poor"
```

This `NA` behavior feeds into how probably can be used with yardstick.
Generally, equivocal values are removed completely from performance
evaluation. So converting them to `NA` and then leaving the default
`na_rm = TRUE` in any yardstick metric removes them from consideration.

``` r
library(yardstick)

# No equivocal zone
segment_logistic_thresh |>
  mutate(.pred_fct = as.factor(.pred)) |>
  precision(Class, .pred_fct)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 precision binary         0.680

# Equivocal zone
segment_pred |>
  mutate(.pred_fct = as.factor(.pred)) |>
  precision(Class, .pred_fct)
#> # A tibble: 1 × 3
#>   .metric   .estimator .estimate
#>   <chr>     <chr>          <dbl>
#> 1 precision binary         0.694
```

As seen above, removing equivocal values using a simple threshold
generally improves performance because the values your model is most
unsure about are removed. But don’t be fooled! You should give those
cases extra consideration, and remember that your reportable rate has
decreased by removing them. In production, you’ll likely have to do
something with those predictions!
