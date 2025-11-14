# Image segmentation predictions

Image segmentation predictions

## Source

Hill, LaPan, Li and Haney (2007). Impact of image segmentation on
high-content screening data quality for SK-BR-3 cells, *BMC
Bioinformatics*, Vol. 8, pg. 340,
<https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-8-340>.

## Value

- segment_naive_bayes,segment_logistic:

  a tibble

## Details

These objects contain test set predictions for the cell segmentation
data from Hill, LaPan, Li and Haney (2007). Each data frame are the
results from different models (naive Bayes and logistic regression).

## Examples

``` r
data(segment_naive_bayes)
data(segment_logistic)
```
