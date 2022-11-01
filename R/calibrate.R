
cal_object <- function(estimates_table, additional_classes = NULL) {
  structure(
    list(
      estimates_table = estimates_table
    ),
    class = c("cal_object", additional_classes)
  )
}

cal_glm_dataframe <- function(.data, truth, estimate, truth_val = 1) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  val_data <- dplyr::mutate(
    .data,
    .is_val = ifelse(!! truth == truth_val, 1, 0),
    .estimate = !! estimate,
  )

  model <- glm(.is_val ~ .estimate, data = val_data, family = "binomial")

  new_estimates <- seq_len(1000) * 0.001
  pred <- predict(model, newdata = data.frame(.estimate = new_estimates), type = "response")

  tbl_estimates <- tibble(
    estimate = new_estimates,
    adj_estimate = pred
  )

  cal_object(tbl_estimates, c("cal_glm", "cal_binary"))
}

probability_bins <- function(.data, truth, estimate, truth_val = 1, no_bins = 10) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  # Creates a case_when entry for each bin
  bin_exprs <- map(
    seq_len(no_bins),
    ~ expr(!! estimate <= !! .x / !! no_bins ~ !! .x)
  )

  # .is_val evaluates if truth is equal to truth val
  # essentially binarizying truth in case is not a binary number
  bin_data <- dplyr::mutate(
    .data,
    .is_val = ifelse(!! truth == truth_val, 1, 0),
    bin = case_when(!!! bin_exprs)
    )

  bin_group <- dplyr::group_by(bin_data, bin)

  bin_summary <- dplyr::summarise(
    bin_group,
    mean_predicted = mean(!! estimate),
    fraction_positives = sum(.is_val) / n()
    )

  # Runs collect() to work with remote connections (ie Spark)
  dplyr::collect(bin_summary)
}
