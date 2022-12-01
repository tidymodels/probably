# Logistic estimates work

    Code
      print(sl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic
      Type: Binary
      Truth: `Class`
      Estimates:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Logistic spline estimates work

    Code
      print(sl_gam)
    Message
      
      -- Probability Calibration 
      Method: Logistic Spline
      Type: Binary
      Truth: `Class`
      Estimates:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Isotonic estimates work

    Code
      print(sl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic
      Type: Binary
      Truth: `Class`
      Estimates:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Isotonic Bootstrapped estimates work

    Code
      print(sl_boot)
    Message
      
      -- Probability Calibration 
      Method: Isotonic Bootstrapped
      Type: Binary
      Truth: `Class`
      Estimates:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Non-default names used for estimate columns

    Code
      cal_estimate_isotonic(new_segment, Class, c(good, poor))
    Message
      
      -- Probability Calibration 
      Method: Isotonic
      Type: Binary
      Truth: `Class`
      Estimates:
      `good` ==> good
      `poor` ==> poor

