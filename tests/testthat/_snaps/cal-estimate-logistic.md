# Logistic estimates work - data.frame

    Code
      print(sl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic regression calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    The selectors in `estimate` resolves to 1 values (".pred_poor") but there are 2 class levels ("good" and "poor").

---

    The `truth` column has 4 levels ("VF", "F", "M", and "L"), but only two-class factors are allowed for this calibration method.

---

    Code
      print(sl_logistic_group)
    Message
      
      -- Probability Calibration 
      Method: Logistic regression calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010, split in 2 groups
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# Logistic estimates work - tune_results

    Code
      print(tl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic regression calibration
      Type: Binary
      Source class: Tune Results
      Data points: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

---

    The `truth` column has 3 levels ("one", "two", and "three"), but only two-class factors are allowed for this calibration method.

# Logistic estimates errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Logistic spline estimates work - data.frame

    Code
      print(sl_gam)
    Message
      
      -- Probability Calibration 
      Method: Generalized additive model calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    Code
      print(sl_gam_group)
    Message
      
      -- Probability Calibration 
      Method: Generalized additive model calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010, split in 2 groups
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# Logistic spline estimates work - tune_results

    Code
      print(tl_gam)
    Message
      
      -- Probability Calibration 
      Method: Generalized additive model calibration
      Type: Binary
      Source class: Tune Results
      Data points: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

# Logistic spline switches to linear if too few unique

    Code
      sl_gam <- cal_estimate_logistic(segment_logistic, Class, smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

---

    Code
      sl_gam <- cal_estimate_logistic(segment_logistic, Class, .by = id, smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

