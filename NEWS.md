# probably 1.1.0

* Significant refactoring of the code underlying the calibration functions. The user-facing APIs have not changed. 

## New Features

* A new set of calibration functions are added that don’t calibrate the model (called `cal_*_none()`). These can be used as a reference implementation when tuning the calibration method (#174). 

* A new function `bound_prediction()` is available to constrain the values of a numeric prediction (#142).

* `cal_estimate_linear()` and `cal_estimate_logistic()` now sets `smooth = FALSE` if data contains too few unique observations for splines to be fit, throwing a warning instead of an error (#167).

* Transition from the magrittr pipe to the base R pipe.

## Bug fixes

* A bug was fixed where `cal_estimate_isotonic()` always used a bootstrap sample.

* Fixed a bug where non-standard names of class probability estimates resulted in an error for some calibration models (#145).

* Bug fix for `cal_plot_breaks()` with binary classification with custom probability column names (#144).

* Fixed an error in `int_conformal_cv()` when grouped resampling was used (#141). 

* Fixed an issue where the `distance` metric appeared inconsistently when using `threshold_perf()` with custom metric sets (@jrwinget, #149).

# probably 1.0.3

* Fixed a bug where the grouping for calibration methods was sensitive to the type of the grouping variables (#127).

# probably 1.0.2

* Quick release to remove the base R pipe (to maintain backwards compatibility). 
# probably 1.0.1

* The conformal functions `int_conformal_infer_*()` were renamed to `int_conformal_*()`.

* `predict.int_conformal_cv()` now returns a `.pred` column that is the average prediction from the resampled models. The prediction intervals are centered on these.

* Split conformal inference is available using `int_conformal_split()` and conformal quantile regression can be used with `int_conformal_quantile()`. 

# probably 1.0.0

* Copyright holder changed to Posit Software PBC.

* A set of calibration tools were added: 

  * The need for calibration can be visualized using the collection of `cal_plot_*()` functions.
  * Calibration methods can be estimated with a family of `cal_estimate_*()` functions.
  * To validate the calibrations using resampling, see the `cal_validate_*()` functions.
  * `cal_apply()` can take a calibration model and apply it to a set of existing predictions.
  
* Possible calibration tools:  

  * Binary classification methods: logistic regression, isotonic regression, and Beta calibration.
  * Multiclass classification: multinomial, isotonic regression, and Beta calibration
  * Regression: linear regression, isotonic regression

* Based on the initial PR (#37) by Antonio R. Vargas, `threshold_perf()` now accepts a custom metric set (#25)

* Two functions were added to compute prediction intervals for regression models via conformal inference: 

  * `int_conformal_infer()`
  * `int_conformal_infer_cv()`

# probably 0.1.0

* Max Kuhn is now the maintainer (#49).

* Re-licensed package from GPL-2 to MIT. All copyright holders are RStudio
  employees and give consent.

* Fixed a bug with how `make_class_pred()` and `make_two_class_pred()` validate
  the `levels` argument (#42).

* `threshold_perf()` now has an explicit `event_level` argument rather than
  respecting the now deprecated `yardstick.event_first` global option (#45).

* Bumped the minimum required R version to >=3.4.0 to align with the rest of the
  tidyverse.
  
* Updated to testthat 3e (#44).

# probably 0.0.6

* `class_pred` objects are now comparable and will be ordered by their levels.
  Equivocal values are generally considered to be the smallest value when
  ordering. `NA` values can be considered smaller if
  `vec_order(na_value = "smallest")` is used.

# probably 0.0.5

* Internal cleanup to be more compatible with vctrs 0.3.0.

# probably 0.0.4

* Suggest the modeldata package, which is where the `lending_club` dataset has been moved after being removed from recipes.

* Use `testthat::verify_output()` on a test expecting a specific vctrs error to avoid failure on CRAN if that error changes in the future.

# probably 0.0.3

* probably has been brought up to date with vctrs 0.2.0. This vctrs update had many function name changes, and required internal refactoring, but there should be minimal external changes.

* The one user facing change comes with casting from one `class_pred` object to another `class_pred`, or to a `factor`. Where previously a warning would be thrown if `x` had levels that did not exist in `to`, an error is now generated. This is consistent with the vctrs behavior when converting from one factor to another.

  ```
  x  <- class_pred(factor("a"))
  to <- class_pred(factor("to"))
  vec_cast(x, to)
  #> Error: Lossy cast from <class_pred> to <class_pred>.
  #> Locations: 1
  ```

# probably 0.0.2

## Bug fixes

* A failing test relying on the R 3.6 change to `sample()` has been corrected.

* An rlang warning in `threshold_perf()` has been fixed.

* A small R 3.1 issue with vctrs has been fixed.

# probably 0.0.1

* First release
