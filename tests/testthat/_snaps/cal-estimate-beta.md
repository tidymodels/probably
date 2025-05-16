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

