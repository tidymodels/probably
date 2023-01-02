# custom metrics

    All metrics must be of type 'class_metric' (e.g. `sensitivity()`, ect)

---

    Code
      segment_logistic %>% threshold_perf(Class, .pred_good, metrics = cls_met_good) %>%
        dplyr::count(.metric)
    Output
      # A tibble: 5 x 2
        .metric      n
        <chr>    <int>
      1 accuracy    21
      2 distance    21
      3 mcc         21
      4 sens        21
      5 spec        21

---

    Code
      segment_logistic %>% threshold_perf(Class, .pred_good, metrics = cls_met_other) %>%
        dplyr::count(.metric)
    Output
      # A tibble: 2 x 2
        .metric      n
        <chr>    <int>
      1 accuracy    21
      2 mcc         21

