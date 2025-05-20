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

