#' @export
calibrate <- function(.data, truth, estimate, models = c("glm")) {
  UseMethod("calibrate")
}

#' @export
calibrate.data.frame <- function(.data, truth, estimate, models = c("glm")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  model_objs <- NULL
  glm_object <- NULL
  if("glm" %in% models) {
    glm_cal <- cal_glm_dataframe(
      .data = .data,
      truth = !! truth,
      estimate = !! estimate
      )
    glm_predict <- cal_add_join(glm_cal, !! estimate, .data = .data)
    glm_probs <- probability_bins(glm_predict, !! truth, .adj_estimate)
    model_objs <- list(
      glm = list(
        title = "GLM",
        calibration = glm_cal,
        performance = glm_probs
      )
    )
  }
  res <- list(
    original = list(
      performance = probability_bins(.data, !! truth, !! estimate)
    ),
    models = model_objs
  )
  class(res) <- "cal_object"
  res
}

#' @export
plot.cal_object <- function(x, ...) {
  model_merge <-  map(
    x$models,
    ~ dplyr::mutate(.x$performance, group = .x$title)
    )

  model_perf <- bind_rows(model_merge)

  merged_data <- dplyr::bind_rows(
    dplyr::mutate(x$original$performance, group = "Original"),
    model_perf
  )
  ggplot(data = merged_data,
         aes(mean_predicted, fraction_positives, color = group, group = group)
         ) +
    geom_line() +
    geom_point() +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = 2, color = "gray") +
    theme_minimal() +
    labs(
      title = "Calibration Plot",
      x = "Mean Predicted",
      y = "Fraction of Positives"
      )
}

cal_add_join <- function(estimates_table, estimate, .data) {
  estimate <- enquo(estimate)
  round_data <- mutate(
    .data,
    .rounded = round(!! estimate, digits = 3)
  )
  matched_data <- dplyr::left_join(
    round_data,
    estimates_table,
    by = c(".rounded" = ".estimate")
  )
  dplyr::select(matched_data, - .rounded)
}

cal_glm_dataframe <- function(.data, truth, estimate, truth_val = 1) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  val_data <- dplyr::mutate(
    .data,
    .is_val = ifelse(!! truth == !! truth_val, 1, 0),
    .estimate = !! estimate,
  )

  model <- glm(.is_val ~ .estimate, data = val_data, family = "binomial")

  new_estimates <- round(seq_len(1000) * 0.001, digits = 3)

  pred <- predict(model, newdata = data.frame(.estimate = new_estimates), type = "response")

  tibble(
    .estimate = new_estimates,
    .adj_estimate = pred
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
