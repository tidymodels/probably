probability_bins <- function(.data, truth, truth_val = 1, estimate, no_bins = 10) {

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
    fraction_positives = sum(.is_val) / n(),
    mean_predicted = mean(!! estimate),
    total_positives = sum(.is_val) ,
    bin_count  = n()
    )

  # Runs collect() to work with remote connections (ie Spark)
  dplyr::collect(bin_summary)
}
