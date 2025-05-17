# no calibration works - data.frame

    Code
      print(nope_reg)
    Message
      
      -- Regression Calibration 
      Method: No calibration
      Source class: Data Frame
      Data points: 2,000
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    Code
      print(nope_reg_group)
    Message
      
      -- Regression Calibration 
      Method: No calibration
      Source class: Data Frame
      Data points: 2,000, split in 2 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

---

    `...` must be empty.
    x Problematic argument:
    * smooth = TRUE

---

    Code
      print(nope_binary)
    Message
      
      -- Probability Calibration 
      Method: No calibration
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

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

---

    Code
      print(nope_multi)
    Message
      
      -- Probability Calibration 
      Method: No calibration
      Type: Multiclass
      Source class: Data Frame
      Data points: 110
      Truth variable: `Species`
      Estimate variables:
      `.pred_bobcat` ==> bobcat
      `.pred_coyote` ==> coyote
      `.pred_gray_fox` ==> gray_fox

---

    x `.by` cannot select more than one column.
    i The following columns were selected:
    i group1 and group2

# no calibration works - tune_results

    Code
      print(nope_reg)
    Message
      
      -- Regression Calibration 
      Method: No calibration
      Source class: Tune Results
      Data points: 750, split in 10 groups
      Truth variable: `outcome`
      Estimate variable: `.pred`

---

    `...` must be empty.
    x Problematic argument:
    * do_something = FALSE

---

    Code
      print(nope_binary)
    Message
      
      -- Probability Calibration 
      Method: No calibration
      Type: Binary
      Source class: Tune Results
      Data points: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

---

    Code
      print(nope_multi)
    Message
      
      -- Probability Calibration 
      Method: No calibration
      Type: Multiclass
      Source class: Tune Results
      Data points: 5,000, split in 10 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_one` ==> one
      `.pred_two` ==> two
      `.pred_three` ==> three

# no calibration fails - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

