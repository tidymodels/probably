# bad inputs to conformal intervals

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
      ! The package "boop" is required.

---

    Code
      basic_obj
    Output
      Conformal inference
      preprocessor: formula 
      model: linear_reg (engine = lm) 
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

    2 repeats were used. This method was developed for basic V-fold cross-validation. Interval coverage is unknown for multiple repeats.

---

    The data were resampled using Bootstrap sampling. This method was developed for V-fold cross-validation. Interval coverage is unknown for your resampling method.

---

    Code
      basic_cv_obj
    Output
      Conformal inference via CV+
      preprocessor: formula 
      model: linear_reg (engine = lm) 
      number of models: 2 
      training set size: 500 
      
      Use `predict(object, new_data, level)` to compute prediction intervals

---

    Code
      int_conformal_infer_cv(workflow())
    Condition
      Error in `int_conformal_infer_cv()`:
      ! No known 'int_conformal_infer_cv' methods for this type of object.

---

    Code
      int_conformal_infer_cv(good_res %>% dplyr::select(-.predictions))
    Condition
      Error in `check_extras()`:
      ! The output must contain a column called '.predictions' that contains the holdout predictions. See the documentation on the 'save_pred' argument of the control function (e.g., `control_grid()` or `control_resamples()`, etc.).

---

    Code
      int_conformal_infer_cv(good_res %>% dplyr::select(-.extracts))
    Condition
      Error in `check_extras()`:
      ! The output must contain a column called '.extracts' that contains the fitted workflow objects. See the documentation on the 'extract' argument of the control function (e.g., `control_grid()` or `control_resamples()`, etc.).

---

    Code
      predict(basic_cv_obj, sim_new[, 3:5])
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `validate_column_names()`:
      ! The following required columns are missing: 'predictor_01', 'predictor_05', 'predictor_06', 'predictor_07', 'predictor_08', 'predictor_09', 'predictor_10', 'predictor_11', 'predictor_12', 'predictor_13', 'predictor_14', 'predictor_15', 'predictor_16', 'predictor_17', 'predictor_18', 'predictor_19', 'predictor_20'.

---

    Code
      probably:::get_root(try(stop("I made you stop"), silent = TRUE),
      control_conformal_infer())
    Condition
      Warning:
      Could not finish the search process due to the following error:
      i I made you stop
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
      

---

    Code
      int_conformal_infer_cv(grid_res, two_models)
    Condition
      Error in `int_conformal_infer_cv()`:
      ! The `parameters` argument selected 2 submodels. Only 1 should be selected.

# conformal control

    Code
      dput(control_conformal_infer())
    Output
      list(method = "iterative", trial_points = 100, var_multiplier = 10, 
          max_iter = 100, tolerance = 0.0001220703125, progress = FALSE, 
          required_pkgs = character(0), seed = 24388L)

---

    Code
      dput(control_conformal_infer(max_iter = 2))
    Output
      list(method = "iterative", trial_points = 100, var_multiplier = 10, 
          max_iter = 2, tolerance = 0.0001220703125, progress = FALSE, 
          required_pkgs = character(0), seed = 59521L)

---

    Code
      control_conformal_infer(method = "rock-paper-scissors")
    Condition
      Error in `control_conformal_infer()`:
      ! `method` must be one of "iterative" or "grid", not "rock-paper-scissors".

