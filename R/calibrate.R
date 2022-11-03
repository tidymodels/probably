# ----------------------------- Object Builders --------------------------------
as_cal_object <- function(estimates, type = NULL, additional_class = NULL) {
  structure(
    list(
      type = type,
      estimates = estimates
    ),
    class = c("cal_object", paste0("cal_", type), additional_class)
  )
}

as_cal_variable <- function(estimate_tbl, estimate, title, model) {
  estimate <- enquo(estimate)
  mod <- list(
    title = title,
    calibration = estimate_tbl
  )
  mod <- set_names(list(mod), model)
  est <- list(mod)
  set_names(est, as_name(estimate))
}

# -------------------------------- GLM -----------------------------------------

#' @export
cal_glm <- function(.data, truth = NULL, estimate = NULL) {
  UseMethod("cal_glm")
}

#' @export
cal_glm.data.frame <- function(.data, truth = NULL, estimate = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_glm_dataframe(.data, !!truth, !!estimate)
    est <- as_cal_variable(res, !! estimate, "GLM", "glm")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, type = type, additional_class = "cal_glm")

}

cal_glm_dataframe <- function(.data, truth, estimate, truth_val = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  # Adds a binary variable if the value of truth is not 0/1,
  # this is to handle Multiclass in the future
  truth_data <- add_is_val(.data, !! truth, truth_val)

  # Creates dummy variable to avoid tidyevel in the formula
  val_data <- dplyr::mutate(truth_data, .estimate = !!estimate)

  model <- glm(.is_val ~ .estimate, data = val_data, family = "binomial")

  # Creates 1,000 predictions using 0 to 1, which become the calibration
  new_estimates <- round(seq_len(1000) * 0.001, digits = 3)
  new_data <- data.frame(.estimate = new_estimates)
  pred <- predict(model, newdata = new_data, type = "response")

  # Returns table with expected names
  tibble(
    .estimate = new_estimates,
    .adj_estimate = pred
  )
}

# ----------------------------- Isotonic ---------------------------------------

#' @export
cal_isotonic <- function(.data, truth = NULL, estimate = NULL) {
  UseMethod("cal_isotonic")
}

#' @export
cal_isotonic.data.frame <- function(.data, truth = NULL, estimate = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_dataframe(.data, !!truth, !!estimate)
    est <- as_cal_variable(res, !! estimate, "Isotonic", "isotonic")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, type = type, additional_class = "cal_isotonic")
}

cal_isoreg_dataframe <- function(.data, truth, estimate, truth_val = NULL,
                                 sampled = FALSE) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_data <- add_is_val(.data, !! truth, truth_val)

  if(sampled) {
    sort_data <- dplyr::slice_sample(truth_data, prop = 1, replace = TRUE)
  } else {
    sort_data <- dplyr::arrange(truth_data, !! estimate)
  }

  model <- isoreg(
    dplyr::pull(sort_data, !!estimate),
    dplyr::pull(sort_data, .is_val)
  )

  model_stepfun <- as.stepfun(model)

  tibble(
    .estimate = environment(model_stepfun)$x,
    .adj_estimate = environment(model_stepfun)$y
  )
}

# ------------------------- Isotonic Bootstrapped-------------------------------

#' @export
cal_isotonic_boot <- function(.data, truth = NULL, estimate = NULL, times = 10) {
  UseMethod("cal_isotonic_boot")
}

#' @export
cal_isotonic_boot.data.frame <- function(.data, truth = NULL, estimate = NULL, times = 10) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_boot(.data, !!truth, !!estimate, times = times)
    est <- as_cal_variable(res, !! estimate, "Isotonic Bootstrapped", "isotonic_boot")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, type = type, additional_class = "cal_isotonic")
}

cal_isoreg_boot <- function(.data, truth, estimate, truth_val = NULL, times = 10) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  seeds <- sample.int(10000, times)
  mods <- purrr::map(
    seeds,
    ~ boot_iso(.data, !!truth, !!estimate, truth_val, .x)
    )
  boot_iso_cal(mods)
}

boot_iso <- function(.data, truth, estimate, truth_val, seed) {
  withr::with_seed(
    seed,
    {
      truth <- enquo(truth)
      estimate <- enquo(estimate)
      cal_isoreg_dataframe(.data, !!truth , !!estimate, truth_val, sampled = TRUE)
    }
  )
}

boot_iso_cal <- function(x) {
  # Creates 1,000 predictions using 0 to 1, which become the calibration
  new_estimates <- round(seq_len(1000) * 0.001, digits = 3)
  new_data <- data.frame(.estimate = new_estimates)
  new_probs <- map(x, ~ cal_add_interval(.x, .estimate, new_data))

  for(i in seq_along(new_probs)) {
    names(new_probs[[i]]) <- c(".estimate", paste0(".adj_", i))
  }

  merge_data <- purrr::reduce(new_probs, inner_join, by = ".estimate")

  adj_data <- dplyr::mutate(
    merge_data,
    .adj_estimate = rowMeans(dplyr::across(dplyr::contains(".adj_")))
    )

  dplyr::select(adj_data, .estimate, .adj_estimate)
}

# -------------------------- Add Adjustment ------------------------------------

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

cal_add_interval <- function(estimates_table, estimate, .data) {
  estimate <- enquo(estimate)
  nd <- dplyr::pull(.data, !!estimate)
  x <- dplyr::pull(estimates_table, .estimate)
  y <- dplyr::pull(estimates_table, .adj_estimate)
  find_interval <- findInterval(nd, x)
  find_interval[find_interval == 0] <- 1
  intervals <- y[find_interval]
  dplyr::mutate(.data, .adj_estimate = intervals)
}

# ------------------------------- Binary ---------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  bins <- cal_get_bins_binary(x)
  cat("Binary Probability Calibration\n")
  cat(" Estimate:", names(x$estimates), "\n")
}

#' @export
plot.cal_binary <- function(x, ...) {

  bins <- cal_get_bins_binary(x)

  model_merge <- map(
    bins,
    ~ {
      .x$bins$Source <- .x$title
      .x$bins
    }
  )

  model_bind <- dplyr::bind_rows(model_merge)

  plot_title <- paste(names(x$estimates), "estimate")

  ggplot(
    data = model_bind,
    aes(mean_predicted, fraction_positives, color = Source, group = Source)
  ) +
    geom_line() +
    geom_point() +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = 2, color = "gray") +
    theme_minimal() +
    labs(
      title = plot_title,
      subtitle = "Calibration Plot",
      x = "Mean Predicted",
      y = "Fraction of Positives"
    )
}

brier_score_binary <- function(.data, truth, estimate) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  subs <- expr(!! truth - !! estimate)
  bs <- dplyr::mutate(.data, subs = !! subs * !! subs)
  bs_sum <- dplyr::summarise(bs, x = sum(subs) / n())
  pull(bs_sum, x)
}

cal_get_bins_binary <- function(x) {
  cals <- map(
    x$estimates[[1]]$calibration,
    ~{
      list(
        title = .x$title,
        bins = .x$performance$probability_bins,
        brier = .x$performance$brier_score
      )
    })

  orig <- x$estimates[[1]]$original
  original <- list(
    title = "Original",
    bins = orig$performance$probability_bins,
    brier = orig$performance$brier_score
  )

  c(list(original = original), cals)
}

# ------------------------------- Utils ----------------------------------------

probability_bins <- function(.data, truth, estimate, truth_val = NULL, no_bins = 10) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_data <- add_is_val(.data, !! truth, truth_val)

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
  truth <- enquo(truth)
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

is_binary_estimate <- function(estimate) {
  TRUE
}

stop_multiclass <- function() {
  stop("Multiclass not supported...yet")
}
