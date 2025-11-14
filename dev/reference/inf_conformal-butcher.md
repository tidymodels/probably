# Butcher methods for conformal inteference intervals

These methods allow you to use the butcher package to reduce the size of
a conformal inference interval object. After calling
[`butcher::butcher()`](https://butcher.tidymodels.org/reference/butcher.html)
on a conformal inference interval object, the only guarantee is that you
will still be able to
[`predict()`](https://rdrr.io/r/stats/predict.html) from that conformal
inference interval object. Other functions may not work as expected.

## Usage

``` r
axe_call.int_conformal_full(x, verbose = FALSE, ...)

axe_ctrl.int_conformal_full(x, verbose = FALSE, ...)

axe_data.int_conformal_full(x, verbose = FALSE, ...)

axe_env.int_conformal_full(x, verbose = FALSE, ...)

axe_fitted.int_conformal_full(x, verbose = FALSE, ...)

axe_call.int_conformal_split(x, verbose = FALSE, ...)

axe_ctrl.int_conformal_split(x, verbose = FALSE, ...)

axe_data.int_conformal_split(x, verbose = FALSE, ...)

axe_env.int_conformal_split(x, verbose = FALSE, ...)

axe_fitted.int_conformal_split(x, verbose = FALSE, ...)

axe_call.int_conformal_quantile(x, verbose = FALSE, ...)

axe_ctrl.int_conformal_quantile(x, verbose = FALSE, ...)

axe_data.int_conformal_quantile(x, verbose = FALSE, ...)

axe_env.int_conformal_quantile(x, verbose = FALSE, ...)

axe_fitted.int_conformal_quantile(x, verbose = FALSE, ...)

axe_call.int_conformal_cv(x, verbose = FALSE, ...)

axe_ctrl.int_conformal_cv(x, verbose = FALSE, ...)

axe_data.int_conformal_cv(x, verbose = FALSE, ...)

axe_env.int_conformal_cv(x, verbose = FALSE, ...)

axe_fitted.int_conformal_cv(x, verbose = FALSE, ...)
```

## Arguments

- x:

  A conformal inference interval object.

- verbose:

  Should information be printed about how much memory is freed from
  butchering?

- ...:

  Extra arguments possibly used by underlying methods.
