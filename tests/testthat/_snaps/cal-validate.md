# Setting summarize to FALSE returns new columns

    Code
      no_res
    Output
      #  10-fold cross-validation 
      # A tibble: 10 x 6
         splits            id     calibration  validation stats_after stats_before    
         <list>            <chr>  <list>       <list>     <list>      <list>          
       1 <split [909/101]> Fold01 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       2 <split [909/101]> Fold02 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       3 <split [909/101]> Fold03 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       4 <split [909/101]> Fold04 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       5 <split [909/101]> Fold05 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       6 <split [909/101]> Fold06 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       7 <split [909/101]> Fold07 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       8 <split [909/101]> Fold08 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
       9 <split [909/101]> Fold09 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>
      10 <split [909/101]> Fold10 <Beta [909]> <tibble>   <tibble>    <tibble [1 x 4]>

# Logistic validation with `fit_resamples`

    'truth' is automaticaly set when this type of object is used.

# Isotonic regression validation with `fit_resamples`

    'truth' is automaticaly set when this type of object is used.

# Bootstrapped isotonic regression validation with `fit_resamples`

    'truth' is automaticaly set when this type of object is used.

# Beta calibration validation with `fit_resamples`

    'truth' is automaticaly set when this type of object is used.

# Multinomial calibration validation with `fit_resamples`

    'truth' is automaticaly set when this type of object is used.

# Linear validation with `fit_resamples`

    'truth' is automaticaly set when this type of object is used.

# validation functions error with tune_results input

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    no applicable method for 'cal_validate_linear' applied to an object of class "c('tune_results', 'tbl_df', 'tbl', 'data.frame')"

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.

---

    This function can only be used with an <rset> object or the results of `tune::fit_resamples()` with a .predictions column.
    i Not an <tune_results> object.
