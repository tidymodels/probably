# ------------------------------------ Apply -----------------------------------
#' @export
cal_apply <- function(.data, calibration, output_name = NULL) {
  UseMethod("cal_apply")
}

#' @export
cal_apply.data.frame <- function(.data, calibration, output_name = NULL){
  cal_add_adjust(calibration, .data, output_name)
}

# -------------------------- Add Adjustment ------------------------------------

cal_add_adjust <- function(calibration, .data, output_name) {
  UseMethod("cal_add_adjust")
}

cal_add_adjust.cal_glm <- function(calibration, .data, output_name) {
  call_add_join_impl(calibration, .data, "glm", output_name)
}

cal_add_adjust.cal_gam <- function(calibration, .data, output_name) {
  call_add_join_impl(calibration, .data, "gam", output_name)
}

cal_add_adjust.cal_isotonic_boot <- function(calibration, .data, output_name) {
  call_add_join_impl(calibration, .data, "isotonic_boot", output_name)
}

cal_add_adjust.cal_isotonic <- function(calibration, .data, output_name) {
  if(calibration$type == "binary") {
    cal <- calibration$estimates[[1]]$isotonic$calibration
    estimate <- names(calibration$estimates[1])
    if(is.null(output_name)) {
      adj_name <- paste0(estimate, "_adj_isotonic")
    } else {
      adj_name <- output_name
    }
    cal_add_interval(cal, !! parse_expr(estimate), .data, !! parse_expr(adj_name))
  }
}

call_add_join_impl <- function(calibration, .data, model, output_name) {
  if(calibration$type == "binary") {
    cal <- calibration$estimates[[1]][[model]]$calibration
    estimate <- names(calibration$estimates[1])
    if(is.null(output_name)) {
      adj_name <- paste0(estimate, "_adj_", model)
    } else {
      adj_name <- output_name
    }
    cal_add_join(cal, !! parse_expr(estimate), .data, !! parse_expr(adj_name))
  }
}

cal_add_join <- function(estimates_table, estimate, .data, adj_name) {
  adj_name <- enquo(adj_name)
  estimate <- enquo(estimate)

  round_data <- mutate(
    .data,
    .rounded := round(!!estimate, digits = 3)
  )

  est_table <- dplyr::rename(estimates_table, !! adj_name := ".adj_estimate")

  matched_data <- dplyr::left_join(
    round_data,
    est_table,
    by = c(".rounded" = ".estimate")
  )

  dplyr::select(matched_data, -.rounded)
}

cal_add_interval <- function(estimates_table, estimate, .data, adj_name) {
  adj_name <- enquo(adj_name)
  estimate <- enquo(estimate)
  nd <- dplyr::pull(.data, !!estimate)
  x <- dplyr::pull(estimates_table, .estimate)
  y <- dplyr::pull(estimates_table, .adj_estimate)
  find_interval <- findInterval(nd, x)
  find_interval[find_interval == 0] <- 1
  intervals <- y[find_interval]
  dplyr::mutate(.data, !!adj_name := intervals)
}

# ----------------------------- Object Builders --------------------------------

as_cal_object <- function(estimates, truth, type, additional_class = NULL) {
  truth <- enquo(truth)
  structure(
    list(
      type = type,
      truth = as_name(truth),
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

  as_cal_object(est, !!truth, type, additional_class = "cal_glm")
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

# -------------------------------- GAM -----------------------------------------

#' @export
cal_gam <- function(.data, truth = NULL, estimate = NULL) {
  UseMethod("cal_gam")
}

#' @export
cal_gam.data.frame <- function(.data, truth = NULL, estimate = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_gam_dataframe(.data, !!truth, !!estimate)
    est <- as_cal_variable(res, !! estimate, "GAM", "gam")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_gam")
}

cal_gam_dataframe <- function(.data, truth, estimate, truth_val = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  # Adds a binary variable if the value of truth is not 0/1,
  # this is to handle Multiclass in the future
  truth_data <- add_is_val(.data, !! truth, truth_val)

  # Creates dummy variable to avoid tidyevel in the formula
  val_data <- dplyr::mutate(truth_data, .estimate = !!estimate)

  model <- gam(.is_val ~ .estimate, data = val_data)

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

  as_cal_object(est, !!truth, type, additional_class = "cal_isotonic")
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
cal_isotonic_boot.data.frame <- function(.data, truth = NULL,
                                         estimate = NULL, times = 10
                                         ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_boot(.data, !!truth, !!estimate, times = times)
    est <- as_cal_variable(res, !! estimate, "Isotonic Bootstrapped", "isotonic_boot")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_isotonic_boot")
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

# ------------------------------ Binary ----------------------------------------


#' @export
cal_binary_bins <- function(.data, truth, estimate, bins = 10) {
  estimate <- enquo(estimate)
  truth <- enquo(truth)
  probability_bins(
    .data = .data,
    truth = !! truth,
    estimate = !! estimate,
    no_bins = bins,
    truth_val = NULL
  )
}

#' @export
cal_binary_plot <- function(.data, truth, estimate, ..., bins = 10) {
  vars <- enquos(...)
  estimate <- enquo(estimate)
  truth <- enquo(truth)

  var_str <- as_name(estimate)

  if(length(vars)) {
    vars_names <- as.character(map(vars, as_name))
    match <- c(vars_names, var_str)
  } else {
    col_names <- colnames(.data)
    match_bol <- substr(col_names, 1, nchar(var_str)) == var_str
    match <- col_names[match_bol]
  }

  match_map <- map(
    match,
    ~ {
      bins_tbl <- cal_binary_bins(.data, !! truth, !!parse_expr(.x), bins = bins)
      dplyr::mutate(bins_tbl, Source = .x)
    })

  matched_tbl <- dplyr::bind_rows(match_map)

  ggplot(
    data = matched_tbl,
    aes(mean_predicted, event_ratio, color = Source, group = Source)
  ) +
    geom_line() +
    geom_point() +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = 2, color = "gray") +
    theme_minimal() +
    labs(
      title = paste0("`", var_str, "` Calibration Plot"),
      x = "Mean Predicted",
      y = "Event Ratio"
    )
}

# ---------------------------- Binary Objs--------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  cat("Probability Calibration\n")
  cat(" --------------------- \n")
  cat("Type: Binary\n")
  cat("Method:", x$estimates[[1]][[1]]$title, "\n")
  cat("Truth:   ", x$truth, "\n")
  cat("Estimate:", names(x$estimates), "\n")
}

brier_score_binary <- function(.data, truth, estimate) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  subs <- expr(!! truth - !! estimate)
  bs <- dplyr::mutate(.data, subs = !! subs * !! subs)
  bs_sum <- dplyr::summarise(bs, x = sum(subs) / n())
  pull(bs_sum, x)
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
    event_ratio = sum(.is_val) / n(),
    events = sum(.is_val),
    bin_total = n()
  )
  bin_ungroup <- dplyr::ungroup(bin_summary)



  # Runs collect() to work with remote connections (ie Spark)
  dplyr::collect(bin_ungroup)


  bin_conf <- map(
    purrr::transpose(bin_ungroup),
    ~ {
      suppressWarnings(pt <- prop.test(.x$events, .x$bin_total))
      ret <- as_tibble(.x)
      ret$conf_low <- pt$conf.int[[1]]
      ret$conf_high <- pt$conf.int[[2]]
      ret
    })
  dplyr::bind_rows(bin_conf)
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
