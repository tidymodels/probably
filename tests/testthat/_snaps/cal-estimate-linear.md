# Linear estimates work - data.frame

    Code
      print(sl_linear)
    Message
      
      -- Regression Calibration 
      Method: Linear calibration
      Source class: Data Frame
      Data points: 2,000
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    Code
      print(sl_linear_group)
    Message
      
      -- Regression Calibration 
      Method: Linear calibration
      Source class: Data Frame
      Data points: 2,000, split in 2 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# Linear estimates work - tune_results

    Code
      print(tl_linear)
    Message
      
      -- Regression Calibration 
      Method: Linear calibration
      Source class: Tune Results
      Data points: 750, split in 10 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

# Linear estimates errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Linear spline estimates work - data.frame

    Code
      print(sl_gam)
    Message
      
      -- Regression Calibration 
      Method: Generalized additive model calibration
      Source class: Data Frame
      Data points: 2,000
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    Code
      print(sl_gam_group)
    Message
      
      -- Regression Calibration 
      Method: Generalized additive model calibration
      Source class: Data Frame
      Data points: 2,000, split in 2 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# Linear spline estimates work - tune_results

    Code
      print(tl_gam)
    Message
      
      -- Regression Calibration 
      Method: Generalized additive model calibration
      Source class: Tune Results
      Data points: 750, split in 10 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

# Linear spline switches to linear if too few unique

    Code
      sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

---

    Code
      sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id,
        smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

---

    Code
      sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

---

    Code
      sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id,
        smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

