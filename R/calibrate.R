#' @export
calibrate <- function(.data, truth, estimate, models = c("glm", "isotonic")) {
  UseMethod("calibrate")
}

#' @export
calibrate.data.frame <- function(.data, truth, estimate,
                                 models = c("glm", "isotonic")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  model_obj <- map(
    models,
    ~ {
      if (.x == "glm") {
        title <- "GLM"
        cal <- cal_glm_dataframe(.data, !!truth, !!estimate)
        preds <- cal_add_join(cal, !!estimate, .data = .data)
      }
      if (.x == "isotonic") {
        title <- "Isotonic"
        cal <- cal_isoreg_dataframe(.data, !!truth, !!estimate)
        preds <- cal_add_interval(cal, !!estimate, .data = .data)
      }
      probs <- probability_bins(preds, !!truth, .adj_estimate)
      list(
        title = title,
        calibration = cal,
        performance = probs
      )
    }
  )

  model_named <- set_names(model_obj, models)

  res <- list(
    original = list(
      performance = probability_bins(.data, !!truth, !!estimate)
    ),
    models = model_named
  )
  class(res) <- "cal_object"
  res
}

#' @export
plot.cal_object <- function(x, ...) {
  model_merge <- map(
    x$models,
    ~ dplyr::mutate(.x$performance, group = .x$title)
  )

  model_perf <- bind_rows(model_merge)

  merged_data <- dplyr::bind_rows(
    dplyr::mutate(x$original$performance, group = "Original"),
    model_perf
  )
  ggplot(
    data = merged_data,
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
    .rounded = round(!!estimate, digits = 3)
  )
  matched_data <- dplyr::left_join(
    round_data,
    estimates_table,
    by = c(".rounded" = ".estimate")
  )
  dplyr::select(matched_data, -.rounded)
}

cal_glm_dataframe <- function(.data, truth, estimate, truth_val = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_data <- add_is_val(.data, truth, truth_val)

  val_data <- dplyr::mutate(truth_data, .estimate = !!estimate)

  model <- glm(.is_val ~ .estimate, data = val_data, family = "binomial")

  new_estimates <- round(seq_len(1000) * 0.001, digits = 3)

  new_data <- data.frame(.estimate = new_estimates)

  pred <- predict(model, newdata = new_data, type = "response")

  tibble(
    .estimate = new_estimates,
    .adj_estimate = pred
  )
}

cal_isoreg_dataframe <- function(.data, truth, estimate, truth_val = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_data <- add_is_val(.data, truth, truth_val)

  model <- isoreg(
    dplyr::pull(truth_data, !!estimate),
    dplyr::pull(truth_data, .is_val)
  )

  model_stepfun <- as.stepfun(model)

  tibble(
    .estimate = environment(model_stepfun)$x,
    .adj_estimate = environment(model_stepfun)$y
  )
}

cal_add_interval <- function(estimates_table, estimate, .data) {
  estimate <- enquo(estimate)

  nd <- dplyr::pull(.data, !!estimate)

  x <- dplyr::pull(estimates_table, .estimate)

  y <- dplyr::pull(estimates_table, .adj_estimate)

  dplyr::mutate(
    .data,
    .adj_estimate = y[findInterval(nd, x)]
  )
}

probability_bins <- function(.data, truth, estimate, truth_val = NULL, no_bins = 10) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_data <- add_is_val(.data, truth, truth_val)

  # Creates a case_when entry for each bin
  bin_exprs <- map(
    seq_len(no_bins),
    ~ expr(!!estimate <= !!.x / !!no_bins ~ !!.x)
  )

  # .is_val evaluates if truth is equal to truth val
  # essentially binarizying truth in case is not a binary number
  bin_data <- dplyr::mutate(truth_data, bin = case_when(!!!bin_exprs))

  bin_group <- dplyr::group_by(bin_data, bin)

  bin_summary <- dplyr::summarise(
    bin_group,
    mean_predicted = mean(!!estimate),
    fraction_positives = sum(.is_val) / n()
  )

  # Runs collect() to work with remote connections (ie Spark)
  dplyr::collect(bin_summary)
}

add_is_val <- function(.data, truth, truth_val = NULL) {
  if (!is.null(truth_val)) {
    ret <- mutate(
      .data,
      .is_val = ifelse(!!truth == !!truth_val, 1, 0)
    )
  } else {
    ret <- mutate(.data, .is_val = !!truth)
  }
  ret
}
