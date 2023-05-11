# Binary breaks functions work

    Code
      testthat_cal_binary() %>% tune::collect_predictions() %>% cal_plot_breaks(class,
        estimate = .pred_class_1)
    Condition
      Error:
      ! The data have several values of '.config' but no 'groups' argument was passed. This will inappropriately pool the data.

# Multi-class breaks functions work

    Code
      testthat_cal_multiclass() %>% tune::collect_predictions() %>% cal_plot_breaks(
        class, estimate = .pred_class_1)
    Condition
      Error:
      ! The data have several values of '.config' but no 'groups' argument was passed. This will inappropriately pool the data.

# breaks plot function errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Binary logistic functions work

    Code
      testthat_cal_binary() %>% tune::collect_predictions() %>% cal_plot_logistic(
        class, estimate = .pred_class_1)
    Condition
      Error:
      ! The data have several values of '.config' but no 'groups' argument was passed. This will inappropriately pool the data.

# logistic plot function errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Binary windowed functions work

    Code
      testthat_cal_binary() %>% tune::collect_predictions() %>% cal_plot_windowed(
        class, estimate = .pred_class_1)
    Condition
      Error:
      ! The data have several values of '.config' but no 'groups' argument was passed. This will inappropriately pool the data.

# windowed plot function errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

# Event level handling works

    Invalid event_level entry: invalid. Valid entries are 'first', 'second', or 'auto'

# regression functions work

    Code
      obj %>% tune::collect_predictions() %>% cal_plot_windowed(outcome, estimate = .pred)
    Condition
      Error:
      ! The data have several values of '.config' but no 'groups' argument was passed. This will inappropriately pool the data.

# regression plot function errors - grouped_df

    x This function does not work with grouped data frames.
    i Apply `dplyr::ungroup()` and use the `.by` argument.

