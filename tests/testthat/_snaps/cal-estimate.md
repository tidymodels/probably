# Logistic estimates work - data.frame

    Code
      print(sl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic
      Type: Binary
      Source class: Data Frame
      Train set size: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Logistic estimates work - tune_results

    Code
      print(tl_logistic)
    Message
      
      -- Probability Calibration 
      Method: Logistic
      Type: Binary
      Source class: Tune Results
      Train set size: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

# Logistic spline estimates work - data.frame

    Code
      print(sl_gam)
    Message
      
      -- Probability Calibration 
      Method: Logistic Spline
      Type: Binary
      Source class: Data Frame
      Train set size: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Logistic spline estimates work - tune_results

    Code
      print(tl_gam)
    Message
      
      -- Probability Calibration 
      Method: Logistic Spline
      Type: Binary
      Source class: Tune Results
      Train set size: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

# Isotonic estimates work - data.frame

    Code
      print(sl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic
      Type: Binary
      Source class: Data Frame
      Train set size: 1,010
      Unique Probability Values: 66
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Isotonic estimates work - tune_results

    Code
      print(tl_isotonic)
    Message
      
      -- Probability Calibration 
      Method: Isotonic
      Type: Binary
      Source class:
      Train set size: 4,000, split in 8 groups
      Unique Probability Values: 86
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

# Isotonic Bootstrapped estimates work

    Code
      print(sl_boot)
    Message
      
      -- Probability Calibration 
      Method: Bootstrapped Isotonic Regression
      Type: Binary
      Source class: Data Frame
      Train set size: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Beta estimates work - data.frame

    Code
      print(sl_beta)
    Message
      
      -- Probability Calibration 
      Method: Beta
      Type: Binary
      Source class: Data Frame
      Train set size: 1,010
      Truth variable: `Class`
      Estimate variables:
      `.pred_good` ==> good
      `.pred_poor` ==> poor

# Beta estimates work - tune_results

    Code
      print(tl_beta)
    Message
      
      -- Probability Calibration 
      Method: Beta
      Type: Binary
      Source class: Tune Results
      Train set size: 4,000, split in 8 groups
      Truth variable: `class`
      Estimate variables:
      `.pred_class_1` ==> class_1
      `.pred_class_2` ==> class_2

# Non-default names used for estimate columns

    Code
      cal_estimate_isotonic(new_segment, Class, c(good, poor))
    Message
      
      -- Probability Calibration 
      Method: Isotonic
      Type: Binary
      Source class: Data Frame
      Train set size: 1,010
      Unique Probability Values: 66
      Truth variable: `Class`
      Estimate variables:
      `good` ==> good
      `poor` ==> poor

