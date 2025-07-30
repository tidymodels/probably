# Logistic validation with data frame input

    There are no saved prediction columns to collect.

# Validation without calibration with data frame input

    There are no saved prediction columns to collect.

# Isotonic classification validation with `fit_resamples`

    `truth` is automatically set when this type of object is used.

# validation functions error with tune_results input

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    no applicable method for 'cal_validate_linear' applied to an object of class "c('tune_results', 'tbl_df', 'tbl', 'data.frame')"

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

# validation sets fail with better message

    Code
      cal_validate_beta(mt_res)
    Condition
      Error in `cal_validate_beta()`:
      ! For validation sets, please make a resampling object from the predictions prior to calling `cal_validate_beta()`

---

    Code
      cal_validate_isotonic(mt_res)
    Condition
      Error in `cal_validate_isotonic()`:
      ! For validation sets, please make a resampling object from the predictions prior to calling `cal_validate_isotonic()`

---

    Code
      cal_validate_isotonic_boot(mt_res)
    Condition
      Error in `cal_validate_isotonic_boot()`:
      ! For validation sets, please make a resampling object from the predictions prior to calling `cal_validate_isotonic_boot()`

---

    Code
      cal_validate_linear(mt_res)
    Condition
      Error in `cal_validate_linear()`:
      ! For validation sets, please make a resampling object from the predictions prior to calling `cal_validate_linear()`

---

    Code
      cal_validate_logistic(mt_res)
    Condition
      Error in `cal_validate_logistic()`:
      ! For validation sets, please make a resampling object from the predictions prior to calling `cal_validate_logistic()`

---

    Code
      cal_validate_multinomial(mt_res)
    Condition
      Error in `cal_validate_multinomial()`:
      ! For validation sets, please make a resampling object from the predictions prior to calling `cal_validate_multinomial()`

---

    Code
      cal_validate_none(mt_res)
    Condition
      Error in `cal_validate_none()`:
      ! For validation sets, please make a resampling object from the predictions prior to calling `cal_validate_none()`

