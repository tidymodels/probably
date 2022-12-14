# Logistic estimates work

    Code
      print(sl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic
      Type: Binary
      Train set size: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Logistic spline estimates work

    Code
      print(sl_gam)
    Message
      
      -- Probability Calibration 
      Method: Logistic Spline
      Type: Binary
      Train set size: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Isotonic estimates work

    Code
      print(sl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic
      Type: Binary
      Train set size: 1,010
      Unique Probability Values: 78
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Isotonic Bootstrapped estimates work

    Code
      print(sl_boot)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped Isotonic Regression
      Type: Binary
      Train set size: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Non-default names used for estimate columns

    Code
      cal_estimate_isotonic(new_segment, Class, c(good, poor))
    Message
      
      -- Probability Calibration 
      Method: Isotonic
      Type: Binary
      Train set size: 1,010
      Unique Probability Values: 78
      Truth variable: `Class`
      Estimate variables:
      `good` ==> good
      `poor` ==> poor

