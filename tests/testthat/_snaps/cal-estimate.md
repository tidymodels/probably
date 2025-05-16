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

# Isotonic estimates work - data.frame

    Code
      print(sl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Unique Predicted Values: 78
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    Code
      print(sl_isotonic_group)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010, split in 2 groups
      Unique Predicted Values: 77
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# Isotonic estimates work - tune_results

    Code
      print(tl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression calibration
      Type: Binary
      Source class: Tune Results
      Data points: 4,000, split in 8 groups
      Unique Predicted Values: 92
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

---

    Code
      print(mtnl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression calibration
      Type: Multiclass (1 v All)
      Source class: Tune Results
      Data points: 5,000, split in 10 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_one` ==> one
      `.pred_two` ==> two
      `.pred_three` ==> three

# Isotonic estimates errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Isotonic linear estimates work - data.frame

    Code
      print(sl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression calibration
      Type: Regression
      Source class: Data Frame
      Data points: 2,000
      Unique Predicted Values: 47
      Truth variable: `outcome`
      Estimate variables:
      `.pred` ==> predictions

---

    Code
      print(sl_logistic_group)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression calibration
      Type: Regression
      Source class: Data Frame
      Data points: 2,000, split in 10 groups
      Unique Predicted Values: 18
      Truth variable: `outcome`
      Estimate variables:
      `.pred` ==> predictions

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# Isotonic Bootstrapped estimates work - data.frame

    Code
      print(sl_boot)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped isotonic regression calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    Code
      print(sl_boot_group)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped isotonic regression calibration
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

# Isotonic Bootstrapped estimates work - tune_results

    Code
      print(tl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped isotonic regression calibration
      Type: Binary
      Source class: Tune Results
      Data points: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

---

    Code
      print(mtnl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped isotonic regression calibration
      Type: Multiclass (1 v All)
      Source class: Tune Results
      Data points: 5,000, split in 10 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_one` ==> one
      `.pred_two` ==> two
      `.pred_three` ==> three

# Isotonic Bootstrapped estimates errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Beta estimates work - data.frame

    Code
      print(sl_beta)
    Message
      
      -- Probability Calibration 
      Method: Beta calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    Code
      print(sl_beta_group)
    Message
      
      -- Probability Calibration 
      Method: Beta calibration
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

# Beta estimates work - tune_results

    Code
      print(tl_beta)
    Message
      
      -- Probability Calibration 
      Method: Beta calibration
      Type: Binary
      Source class: Tune Results
      Data points: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

---

    Code
      print(mtnl_beta)
    Message
      
      -- Probability Calibration 
      Method: Beta calibration
      Type: Multiclass (1 v All)
      Source class: Tune Results
      Data points: 5,000, split in 10 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_one` ==> one
      `.pred_two` ==> two
      `.pred_three` ==> three

# Beta estimates errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Multinomial estimates work - data.frame

    Code
      print(sp_multi)
    Message
      
      -- Probability Calibration 
      Method: Multinomial regression calibration
      Type: Multiclass
      Source class: Data Frame
      Data points: 110
      Truth variable: `Species`
      Estimate variables:
      `.pred_bobcat` ==> bobcat
      `.pred_coyote` ==> coyote
      `.pred_gray_fox` ==> gray_fox

---

    Code
      print(sp_smth_multi)
    Message
      
      -- Probability Calibration 
      Method: Generalized additive model calibration
      Type: Multiclass
      Source class: Data Frame
      Data points: 110
      Truth variable: `Species`
      Estimate variables:
      `.pred_bobcat` ==> bobcat
      `.pred_coyote` ==> coyote
      `.pred_gray_fox` ==> gray_fox

---

    Code
      print(sl_multi_group)
    Message
      
      -- Probability Calibration 
      Method: Multinomial regression calibration
      Type: Multiclass
      Source class: Data Frame
      Data points: 110, split in 2 groups
      Truth variable: `Species`
      Estimate variables:
      `.pred_bobcat` ==> bobcat
      `.pred_coyote` ==> coyote
      `.pred_gray_fox` ==> gray_fox

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# Multinomial estimates work - tune_results

    Code
      print(tl_multi)
    Message
      
      -- Probability Calibration 
      Method: Multinomial regression calibration
      Type: Multiclass
      Source class: Tune Results
      Data points: 5,000, split in 10 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_one` ==> one
      `.pred_two` ==> two
      `.pred_three` ==> three

---

    Code
      print(tl_smth_multi)
    Message
      
      -- Probability Calibration 
      Method: Generalized additive model calibration
      Type: Multiclass
      Source class: Tune Results
      Data points: 5,000, split in 10 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_one` ==> one
      `.pred_two` ==> two
      `.pred_three` ==> three

# Multinomial estimates errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

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

# Multinomial spline switches to linear if too few unique

    Code
      sl_gam <- cal_estimate_multinomial(smol_species_probs, Species, smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

---

    Code
      sl_gam <- cal_estimate_multinomial(smol_by_species_probs, Species, .by = id,
        smooth = TRUE)
    Condition
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.
      Warning:
      Too few unique observations for spline-based calibrator. Setting `smooth = FALSE`.

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

# Non-default names used for estimate columns

    Code
      cal_estimate_isotonic(new_segment, Class, c(good, poor))
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression calibration
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Unique Predicted Values: 78
      Truth variable: `Class`
      Estimate variables:
      `good` ==> good
      `poor` ==> poor

