# Boosted regression trees predictions

Boosted regression trees predictions

## Value

- boosting_predictions_oob,boosting_predictions_test:

  tibbles

## Details

These data have a set of holdout predictions from 10-fold
cross-validation and a separate collection of test set predictions from
the same boosted tree model. The data were generated using the
`sim_regression` function in the modeldata package.

## Examples

``` r
data(boosting_predictions_oob)
#> Warning: data set ‘boosting_predictions_oob’ not found
str(boosting_predictions_oob)
#> tibble [2,000 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ outcome: num [1:2000] -13.45 43.85 6.14 26.85 -7.43 ...
#>  $ .pred  : num [1:2000] 3.13 32.37 10.3 17.79 12.28 ...
#>  $ id     : chr [1:2000] "Fold01" "Fold01" "Fold01" "Fold01" ...
str(boosting_predictions_test)
#> tibble [500 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ outcome: num [1:500] -4.65 1.12 14.7 36.28 14.08 ...
#>  $ .pred  : num [1:500] 4.12 1.83 13.05 19.07 14.93 ...
```
