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
