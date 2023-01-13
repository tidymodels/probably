# Setting summarize to FALSE returns new columns

    Code
      no_res
    Output
      #  10-fold cross-validation 
      # A tibble: 10 x 6
         splits            id     calibration  validation         stats_after stats_~1
         <list>            <chr>  <list>       <list>             <list>      <list>  
       1 <split [909/101]> Fold01 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       2 <split [909/101]> Fold02 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       3 <split [909/101]> Fold03 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       4 <split [909/101]> Fold04 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       5 <split [909/101]> Fold05 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       6 <split [909/101]> Fold06 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       7 <split [909/101]> Fold07 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       8 <split [909/101]> Fold08 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
       9 <split [909/101]> Fold09 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
      10 <split [909/101]> Fold10 <Beta [909]> <tibble [101 x 4]> <tibble>    <tibble>
      # ... with abbreviated variable name 1: stats_before

