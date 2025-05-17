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

