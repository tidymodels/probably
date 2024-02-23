# Logistic estimates work - data.frame

    Code
      print(sl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic regression
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    We can't connect the specified prediction columns to some factor levels (good). The selected columns were .pred_poor. Are there more columns to add in the function call?

---

    The number of outcome factor levels isn't consistent with the calibration method. Only two class `truth` factors are allowed. The given levels were: 'VF', 'F', 'M', 'L'

---

    Code
      print(sl_logistic_group)
    Message
      
      -- Probability Calibration 
      Method: Logistic regression
      Type: Binary
      Source class: Data Frame
      Data points: 1,010, split in 2 groups
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    x `.by` cannot select more than one column.
    i The following 2 columns were selected:
    i group1 and group2

# Logistic estimates work - tune_results

    Code
      print(tl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic regression
      Type: Binary
      Source class: Tune Results
      Data points: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

---

    The number of outcome factor levels isn't consistent with the calibration method. Only two class `truth` factors are allowed. The given levels were: '.pred_one', '.pred_two', '.pred_three'

# Logistic estimates errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Logistic spline estimates work - data.frame

    Code
      print(sl_gam)
    Message
      
      -- Probability Calibration 
      Method: Generalized additive model
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
      Method: Generalized additive model
      Type: Binary
      Source class: Data Frame
      Data points: 1,010, split in 2 groups
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    x `.by` cannot select more than one column.
    i The following 2 columns were selected:
    i group1 and group2

# Logistic spline estimates work - tune_results

    Code
      print(tl_gam)
    Message
      
      -- Probability Calibration 
      Method: Generalized additive model
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
      Method: Isotonic regression
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Unique Predicted Values: 66
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    Code
      print(sl_isotonic_group)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression
      Type: Binary
      Source class: Data Frame
      Data points: 1,010, split in 2 groups
      Unique Predicted Values: 59
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    x `.by` cannot select more than one column.
    i The following 2 columns were selected:
    i group1 and group2

# Isotonic estimates work - tune_results

    Code
      print(tl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression
      Type: Binary
      Source class:
      Data points: 4,000, split in 8 groups
      Unique Predicted Values: 86
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

---

    Code
      print(mtnl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression
      Type: Multiclass (1 v All)
      Source class:
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
      Method: Isotonic regression
      Type: Regression
      Source class: Data Frame
      Data points: 2,000
      Unique Predicted Values: 43
      Truth variable: `outcome`
      Estimate variables:
      `.pred` ==> predictions

---

    Code
      print(sl_logistic_group)
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression
      Type: Regression
      Source class: Data Frame
      Data points: 2,000, split in 10 groups
      Unique Predicted Values: 11
      Truth variable: `outcome`
      Estimate variables:
      `.pred` ==> predictions

---

    x `.by` cannot select more than one column.
    i The following 2 columns were selected:
    i group1 and group2

# Isotonic Bootstrapped estimates work - data.frame

    Code
      print(sl_boot)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped isotonic regression
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
      Method: Bootstrapped isotonic regression
      Type: Binary
      Source class: Data Frame
      Data points: 1,010, split in 2 groups
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

---

    x `.by` cannot select more than one column.
    i The following 2 columns were selected:
    i group1 and group2

# Isotonic Bootstrapped estimates work - tune_results

    Code
      print(tl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped isotonic regression
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
      Method: Bootstrapped isotonic regression
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
    i The following 2 columns were selected:
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
      print(mtnl_isotonic)
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
      Method: Multinomial regression
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
      Method: Generalized additive model
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
      Method: Multinomial regression
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
    i The following 2 columns were selected:
    i group1 and group2

# Multinomial estimates work - tune_results

    Code
      print(tl_multi)
    Message
      
      -- Probability Calibration 
      Method: Multinomial regression
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
      Method: Generalized additive model
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

# Linear estimates work - data.frame

    Code
      print(sl_logistic)
    Message
      
      -- Regression Calibration 
      Method: Linear
      Source class: Data Frame
      Data points: 2,000
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    Code
      print(sl_logistic_group)
    Message
      
      -- Regression Calibration 
      Method: Linear
      Source class: Data Frame
      Data points: 2,000, split in 2 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    x `.by` cannot select more than one column.
    i The following 2 columns were selected:
    i group1 and group2

# Linear estimates work - tune_results

    Code
      print(tl_linear)
    Message
      
      -- Regression Calibration 
      Method: Linear
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
      Method: Generalized additive model
      Source class: Data Frame
      Data points: 2,000
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    Code
      print(sl_gam_group)
    Message
      
      -- Regression Calibration 
      Method: Generalized additive model
      Source class: Data Frame
      Data points: 2,000, split in 2 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    x `.by` cannot select more than one column.
    i The following 2 columns were selected:
    i group1 and group2

# Linear spline estimates work - tune_results

    Code
      print(tl_gam)
    Message
      
      -- Regression Calibration 
      Method: Generalized additive model
      Source class: Tune Results
      Data points: 750, split in 10 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

# Non-default names used for estimate columns

    Code
      cal_estimate_isotonic(new_segment, Class, c(good, poor))
    Message
      
      -- Probability Calibration 
      Method: Isotonic regression
      Type: Binary
      Source class: Data Frame
      Data points: 1,010
      Unique Predicted Values: 66
      Truth variable: `Class`
      Estimate variables:
      `good` ==> good
      `poor` ==> poor

