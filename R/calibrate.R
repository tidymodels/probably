
cal_add_adj <- function(.data, cal_object) {
  estimates <- cal_get_estimates(cal_object)
  col_names <- colnames(estimates)
  match_col <- col_names[[1]]
  round_data <- mutate(
    .data,
    .rounded = round(!! parse_expr(match_col), digits = 3)
  )
  matched_data <- dplyr::left_join(
    round_data,
    estimates,
    by = c(".rounded" = match_col)
  )
  dplyr::select(matched_data, - .rounded)
}

cal_object <- function(estimates_table, truth, truth_val,
                       additional_classes = NULL
                       ) {
  structure(
    list(
      truth = truth,
      truth_val = truth_val,
      estimates_table = estimates_table
    ),
    class = c("cal_object", additional_classes)
  )
}

cal_get_estimates <- function(x) {
  x$estimates_table
}

cal_glm_dataframe <- function(.data, truth, estimate, truth_val = 1) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  adj_name <- parse_expr(paste0(as_name(estimate), "_adj_glm"))

  val_data <- dplyr::mutate(
    .data,
    .is_val = ifelse(!! truth == truth_val, 1, 0),
    .estimate = !! estimate,
  )

  model <- glm(.is_val ~ .estimate, data = val_data, family = "binomial")

  new_estimates <- round(seq_len(1000) * 0.001, digits = 3)
  pred <- predict(model, newdata = data.frame(.estimate = new_estimates), type = "response")

  tbl_estimates <- tibble(
    !! estimate := new_estimates,
    !! adj_name := pred
  )

  cal_object(
    estimates_table = tbl_estimates,
    truth = truth,
    truth_val = truth_val,
    additional_classes = c("cal_glm", "cal_binary")
    )
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
