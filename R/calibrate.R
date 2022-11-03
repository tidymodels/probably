# --------------------------------- Calibrate ----------------------------------
#' @export
cal_multi <- function(.data, truth, estimate,
                      models = c("glm", "isotonic", "isotonic_boot")
                      ) {
  UseMethod("cal_multi")
}

#' @export
cal_multi.data.frame <- function(.data, truth, estimate,
                                 models = c("glm", "isotonic", "isotonic_boot")
                                 ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  calibrate_impl(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    models = models
  )
}

calibrate_impl <- function(.data, truth, estimate, models) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  # TODO - Better condition to determine if binary (this is a temp example code)
  if(length(estimate) == 2) {
    type <- "binary"
    estimates <- calibrate_variable(
      .data = .data,
      truth = !! truth,
      estimate = !! estimate,
      models = models
      )
  } else {
    type <- "multiclass"
    stop("Multiclass not supported...yet :)")
  }
  as_cal_object(estimates, type, "cal_multiple")
}

calibrate_variable <- function(.data, truth, estimate, models) {

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
      if (.x == "isotonic_boot") {
        title <- "Isotonic Bootstrapped"
        cal <- cal_isoreg_boot(.data, !!truth, !!estimate)
        preds <- cal_add_join(cal, !!estimate, .data = .data)
      }
      bs <- brier_score(preds, !!truth, .adj_estimate)
      probs <- probability_bins(preds, !!truth, .adj_estimate)
      list(
        title = title,
        calibration = cal,
        performance = list(
          probability_bins = probs,
          brier_score = bs
        )

      )
    }
  )

  model_named <- set_names(model_obj, models)

  res <- list()

  cal <- list(
    original = list(
      performance = list(
        probability_bins = probability_bins(.data, !!truth, !!estimate),
        brier_score = brier_score(.data, !!truth, !!estimate)
      )
    ),
    calibration = model_named
  )
  res$cs <- cal
  set_names(res, as_name(estimate))
}

as_cal_object <- function(estimates, type = NULL, additional_class = NULL) {
  structure(
    list(
      type = type,
      estimates = estimates
    ),
    class = c("cal_object", additional_class, paste0("cal_", type))
  )
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

  calibrate_impl(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    models = "glm"
  )
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

  calibrate_impl(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    models = "isotonic"
  )
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

  calibrate_impl(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    models = "isotonic_boot"
  )
}

cal_isoreg_boot <- function(.data, truth, estimate, truth_val = NULL, times = 10) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  seeds <- sample.int(10000, times)
  mods <- purrr::map(seeds, ~ boot_iso(.data, !!truth, !!estimate, truth_val, .x))
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
  cat("  Brier Scores:\n")
  map(
    bins,
    ~ {
      title <- paste0(.x$title, ":")
      cat("   |--", title, round(.x$brier, 4), "\n")
    }
  )
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

brier_score <- function(.data, truth, estimate) {
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
