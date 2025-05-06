# custom metrics

    All metrics must be of type 'class_metric' (e.g. `sensitivity()`, ect)

---

    Code
      dplyr::count(threshold_perf(segment_logistic, Class, .pred_good, metrics = cls_met_good),
      .metric)
    Output
      # A tibble: 5 x 2
        .metric         n
        <chr>       <int>
      1 accuracy       21
      2 distance       21
      3 mcc            21
      4 sensitivity    21
      5 specificity    21

---

    Code
      dplyr::count(threshold_perf(segment_logistic, Class, .pred_good, metrics = cls_met_other),
      .metric)
    Output
      # A tibble: 2 x 2
        .metric      n
        <chr>    <int>
      1 accuracy    21
      2 mcc         21

