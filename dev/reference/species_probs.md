# Predictions on animal species

Predictions on animal species

## Source

Reid, R. E. B. (2015). A morphometric modeling approach to
distinguishing among bobcat, coyote and gray fox scats. *Wildlife
Biology*, 21(5), 254-262

## Value

- species_probs:

  a tibble

## Details

These data are holdout predictions from resampling for the animal scat
data of Reid (2015) based on a C5.0 classification model.

## Examples

``` r
data(species_probs)
str(species_probs)
#> tibble [110 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ Species       : Factor w/ 3 levels "bobcat","coyote",..: 3 3 1 3 3 1 1 1 1 1 ...
#>  $ .pred_bobcat  : num [1:110] 0.0976 0.1548 0.5007 0.2563 0.4627 ...
#>  $ .pred_coyote  : num [1:110] 0.053 0.139 0.088 0 0.287 ...
#>  $ .pred_gray_fox: num [1:110] 0.849 0.706 0.411 0.744 0.25 ...
```
