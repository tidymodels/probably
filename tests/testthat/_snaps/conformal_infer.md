# bad inputs to conformal intervals

    Code
      basic_obj
    Output
      Conformal inference
      training set size: 500 
      
      Use `predict(object, new_data, level)` to compute prediction intervals

---

    Code
      int_conformal_infer(workflow(), sim_new)
    Condition
      Error in `int_conformal_infer()`:
      ! 'object' should be a fitted workflow object.

---

    Code
      int_conformal_infer(wflow %>% extract_fit_parsnip(), sim_new)
    Condition
      Error in `int_conformal_infer()`:
      ! No known 'int_conformal_infer' methods for this type of object.

---

    Code
      int_conformal_infer(wflow_cls, sim_cls_new)
    Condition
      Error in `int_conformal_infer()`:
      ! 'object' should be a regression model.

---

    Code
      predict(basic_obj, sim_new[, 3:5])
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'predictor_01', 'predictor_05', 'predictor_06', 'predictor_07', 'predictor_08', 'predictor_09', 'predictor_10', 'predictor_11', 'predictor_12', 'predictor_13', 'predictor_14', 'predictor_15', 'predictor_16', 'predictor_17', 'predictor_18', 'predictor_19', 'predictor_20'.

---

    Code
      int_conformal_infer(wflow, train_data = sim_cls_data)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'predictor_01', 'predictor_02', 'predictor_03', 'predictor_04', 'predictor_05', 'predictor_06', 'predictor_07', 'predictor_08', 'predictor_09', 'predictor_10', 'predictor_11', 'predictor_12', 'predictor_13', 'predictor_14', 'predictor_15', 'predictor_16', 'predictor_17', 'predictor_18', 'predictor_19', 'predictor_20'.

---

    Code
      int_conformal_infer(wflow, sim_new)
    Condition
      Error in `int_conformal_infer()`:
      ! The model to estimate the possible interval length failed with the following message:
      i A term has fewer unique covariate combinations than specified maximum degrees of freedom

---

    Code
      int_conformal_infer(wflow, sim_data, control = control_conformal_infer(
        required_pkgs = "boop"))
    Condition
      Error in `int_conformal_infer()`:
      ! The package `boop` is required.

---

    Code
      get_root(try(stop("ope!"), silent = TRUE), control_conformal_infer())
    Condition
      Warning:
      Could not finish the search process due to the following error:
      i ope!
    Output
      [1] NA

# conformal intervals

    Code
      res_small <- predict(smol_obj, sim_new)
    Condition
      Warning in `stats::uniroot()`:
      _NOT_ converged in 2 iterations
      Warning in `stats::uniroot()`:
      _NOT_ converged in 2 iterations
      Warning:
      Search did not converge.
      Warning:
      Search did not converge.
      Warning in `stats::uniroot()`:
      _NOT_ converged in 2 iterations
      Warning in `stats::uniroot()`:
      _NOT_ converged in 2 iterations
      Warning:
      Search did not converge.
      Warning:
      Search did not converge.
    Output
      

# conformal control

    Code
      dput(control_conformal_infer())
    Output
      list(method = "search", trial_points = 100, var_multiplier = 10, 
          max_iter = 100, tolerance = 0.0001220703125, progress = FALSE, 
          required_pkgs = character(0), seed = 24388L)

---

    Code
      dput(control_conformal_infer(max_iter = 2))
    Output
      list(method = "search", trial_points = 100, var_multiplier = 10, 
          max_iter = 2, tolerance = 0.0001220703125, progress = FALSE, 
          required_pkgs = character(0), seed = 59521L)

---

    Code
      control_conformal_infer(method = "rock-paper-scissors")
    Condition
      Error in `control_conformal_infer()`:
      ! `method` must be one of "search" or "grid", not "rock-paper-scissors".

