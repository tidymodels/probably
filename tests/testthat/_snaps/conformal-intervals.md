# bad inputs to conformal intervals

    'object' should be a fitted workflow object.

---

    No known 'int_conformal_infer' methods for this type of object.

---

    The following required columns are missing: 'predictor_01', 'predictor_05', 'predictor_06', 'predictor_07', 'predictor_08', 'predictor_09', 'predictor_10', 'predictor_11', 'predictor_12', 'predictor_13', 'predictor_14', 'predictor_15', 'predictor_16', 'predictor_17', 'predictor_18', 'predictor_19', 'predictor_20'.

---

    The following required columns are missing: 'predictor_01', 'predictor_02', 'predictor_03', 'predictor_04', 'predictor_05', 'predictor_06', 'predictor_07', 'predictor_08', 'predictor_09', 'predictor_10', 'predictor_11', 'predictor_12', 'predictor_13', 'predictor_14', 'predictor_15', 'predictor_16', 'predictor_17', 'predictor_18', 'predictor_19', 'predictor_20'.

# conformal intervals

    Code
      res_small <- int_conformal_infer(wflow_small, sim_new, train_data = sim_data,
        control = ctrl)
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
          seed = 24388L)

---

    Code
      dput(control_conformal_infer(max_iter = 2))
    Output
      list(method = "search", trial_points = 100, var_multiplier = 10, 
          max_iter = 2, tolerance = 0.0001220703125, progress = FALSE, 
          seed = 59521L)

---

    `method` must be one of "search" or "grid", not "rock-paper-scissors".
