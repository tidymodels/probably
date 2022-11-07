# ------------------------------------ Apply -----------------------------------
#' @export
cal_apply <- function(x, calibration) {
  UseMethod("cal_apply")
}

#' @export
cal_apply.data.frame <- function(x, calibration){
  if(calibration$type == "binary") {
    truth <- calibration$truth
    estimate <- names(calibration$estimates[1])
    x <- dplyr::select(x, !!parse_expr(truth), !!parse_expr(estimate))
  }
  ca <- cal_add_adjust(calibration, x)
  as_cal_applied(ca, calibration)
}

#' @export
cal_apply.cal_applied <- function(x, calibration){
  if(calibration$type == "binary") {
    cal <- x$calibration[[1]]$table
    desc <- x$calibration[[1]]$desc
    ca <- cal_add_adjust(calibration, cal, desc = desc)
    as_cal_applied(ca, calibration)
  }
}

as_cal_applied <- function(results, calibration) {
  new_name <- paste0(".adj_", length(colnames(results)) - 1)
  structure(
    list(
      truth = calibration$truth,
      type = calibration$type,
      calibration = results
    ),
    class = c("cal_applied", paste0("cal_applied_", calibration$type))
  )
}

#' @export
print.cal_applied <- function(x, ...) {
  cat("Cal Applied")
}

#' @export
plot.cal_applied_binary <- function(x, ...) {
  tibble::as_tibble(x) %>%
    dplyr::group_by(.source) %>%
    cal_binary_plot()
}

# -------------------------- Add Adjustment ------------------------------------

cal_add_adjust <- function(calibration, .data, desc = NULL) {
  UseMethod("cal_add_adjust")
}

cal_add_adjust.cal_glm <- function(calibration, .data, desc = NULL) {
  call_add_join_impl(calibration, .data, "glm", desc = desc)
}

cal_add_adjust.cal_gam <- function(calibration, .data, desc = NULL) {
  call_add_join_impl(calibration, .data, "gam", desc = desc)
}

cal_add_adjust.cal_isotonic_boot <- function(calibration, .data, desc = NULL) {
  call_add_join_impl(calibration, .data, "isotonic_boot", desc = desc)
}

cal_add_adjust.cal_isotonic <- function(calibration, .data, desc = NULL) {
  ret <- list()
  if(calibration$type == "binary") {
    estimate <- calibration$estimates[1]
    ret <- estimate[[1]][["isotonic"]]
    cal <- ret$calibration
    est_name <- names(estimate)
    adj_name <- paste0(".adj_", length(colnames(.data)) - 2)
    x <- cal_add_interval(cal, !! parse_expr(est_name), .data, !! parse_expr(adj_name), desc = desc)
    ret <- as_cal_res(x, ret$title, adj_name, desc, est_name)
  }
  ret

}

cal_add_interval <- function(estimates_table, estimate, .data, adj_name, desc = NULL) {
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

call_add_join_impl <- function(calibration, .data, model, desc = NULL) {
  ret <- list()
  if(calibration$type == "binary") {
    estimate <- calibration$estimates[1]
    ret <- estimate[[1]][[model]]
    cal <- ret$calibration
    est_name <- names(estimate)
    adj_name <- paste0(".adj_", length(colnames(.data)) - 2)
    x <- cal_add_join(cal, !! parse_expr(est_name), .data, !! parse_expr(adj_name))
    ret <- as_cal_res(x, ret$title, adj_name, desc, est_name)
  }
  ret
}

as_cal_res <- function(x, title, adj_name, desc, var_name) {
  desc_table <- tibble(.source = title, .column = adj_name)
  desc <- dplyr::bind_rows(desc, desc_table)
  ret <- list(
    cs = list(
      table = x,
      desc = desc
    )
  )
  names(ret) <- var_name
  ret
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

#' @importFrom tibble as_tibble
#' @export
as_tibble.cal_applied_binary <- function(x, ...) {
  tbl <- x$calibration[[1]]$table
  desc <- x$calibration[[1]]$desc

  desc <- dplyr::bind_rows(
    desc,
    tibble(
      .source = names(x$calibration[1]),
      .column = names(x$calibration[1])
      )
  )

  tbl_map <- map(tbl, ~.x)
  tbl_mapped <- map(
    2:ncol(tbl),
    ~{
      tibble(
        .column = names(tbl_map[.x]),
        .truth = tbl_map[[1]],
        .estimate = tbl_map[[.x]]
      )
    }
  )
  tbl_bind <- dplyr::bind_rows(tbl_mapped)
  tbl_merged <- dplyr::left_join(tbl_bind, desc, by = ".column")
  dplyr::select(tbl_merged, .source, .truth, .estimate)
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
    res <- cal_model_impl(.data, !!truth, !!estimate, method = "glm")
    est <- as_cal_variable(res, !! estimate, "GLM", "glm")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_glm")
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
    res <- cal_model_impl(.data, !!truth, !!estimate, method = "gam")
    est <- as_cal_variable(res, !! estimate, "GAM", "gam")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_gam")
}

# -------------------------------- Beta ----------------------------------------

#' @export
cal_beta <- function(.data, truth = NULL, estimate = NULL) {
  UseMethod("cal_beta")
}

#' @export
cal_beta.data.frame <- function(.data, truth = NULL, estimate = NULL) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_model_impl(.data, !!truth, !!estimate, method = "beta")
    est <- as_cal_variable(res, !! estimate, "Beta", "beta")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_beta")
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
cal_binary_plot <- function(.data, truth = .truth, estimate = .estimate, bins = 10) {
  estimate <- enquo(estimate)
  truth <- enquo(truth)

  bin_tbl <- cal_binary_bins(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate
  )

  grouping <- dplyr::group_vars(.data)
  if(length(grouping)) {
    grouping <- parse_expr(grouping)
  } else {
    grouping <- NULL
  }

  ggplot(
    data = bin_tbl,
    aes(x = predicted_midpoint,
        y = event_rate,
        color = !! grouping,
        group = !! grouping,
        ymin = conf_low,
        ymax = conf_high
        )
    ) +
    geom_line() +
    geom_point(alpha = 0.5) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = 2, color = "gray") +
    #geom_errorbar(width = 0.02, alpha = 0.5) +
    theme_minimal() +
    labs(
      title = "Calibration Plot",
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

cal_model_impl <- function(.data, truth, estimate, truth_val = NULL, method) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  # Adds a binary variable if the value of truth is not 0/1,
  # this is to handle Multiclass in the future
  truth_data <- add_is_val(.data, !! truth, truth_val)

  # Creates dummy variable to avoid tidyevel in the formula
  val_data <- dplyr::mutate(truth_data, .estimate = !!estimate)

  if(method == "gam") model <- gam::gam(.is_val ~ .estimate, data = val_data)
  if(method == "glm") model <- glm(.is_val ~ .estimate, data = val_data, family = "binomial")
  if(method == "beta") model <- betareg::betareg(.estimate ~ .is_val, data = val_data)

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

probability_bins <- function(.data, truth, estimate, truth_val = NULL, no_bins = 10) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_data <- add_is_val(.data, !! truth, truth_val)

  # Creates a case_when entry for each bin
  bin_exprs <- map(
    seq_len(no_bins),
    ~ expr(!!estimate <= !!.x / !!no_bins ~ !!.x)
  )

  bin_data <- truth_data %>%
    dplyr::mutate(.bin = case_when(!!!bin_exprs)) %>%
    dplyr::group_by(.bin, .add = TRUE) %>%
    dplyr::summarise(
      predicted_midpoint = median(!!estimate),
      event_rate = sum(.is_val) / n(),
      events = sum(.is_val),
      total = n()
    ) %>%
    dplyr::ungroup()

  purrr::map_df(
    purrr::transpose(bin_data),
    ~ {
      suppressWarnings(pt <- prop.test(.x$events, .x$total))
      ret <- as_tibble(.x)
      ret$conf_low <- pt$conf.int[[1]]
      ret$conf_high <- pt$conf.int[[2]]
      ret
    })

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
