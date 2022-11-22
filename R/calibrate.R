# ------------------------------------ Apply -----------------------------------
#' @export
cal_apply <- function(x, calibration) {
  UseMethod("cal_apply")
}

#' @export
cal_apply.data.frame <- function(x, calibration){
  if(calibration$type == "binary") {
    cal_add_adjust(calibration, x)
  }
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

cal_add_adjust.cal_logistic <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data,
    model = "glm"
    )
}

cal_add_adjust.cal_logistic_spline <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data,
    model = "logistic_spline"
  )
}

cal_add_adjust.cal_isotonic_boot <- function(calibration, .data, desc = NULL) {
  cal_add_join_impl(calibration, .data, "isotonic_boot", desc = desc)
}

cal_add_adjust.cal_isotonic <- function(calibration, .data, desc = NULL) {
  ret <- list()
  if(calibration$type == "binary") {
    estimate <- calibration$estimates[1]
    ret <- estimate[[1]][["isotonic"]]
    cal <- ret$calibration
    est_name <- names(estimate)
    adj_name <- paste0(".adj_", length(colnames(.data)) - 2)
    x <- cal_add_interval(
      estimates_table = cal,
      estimate = !! parse_expr(est_name),
      .data = .data,
      adj_name = !! parse_expr(adj_name),
      desc = desc
      )
    ret <- as_cal_res(x, ret$title, adj_name, desc, est_name)
  }
  ret

}

cal_add_interval <- function(estimates_table, estimate, .data, adj_name, desc = NULL) {
    estimate <- enquo(estimate)
    y <- estimates_table$.adj_estimate
    find_interval <- findInterval(
      dplyr::pull(.data, !!estimate),
      estimates_table$.estimate
      )
    find_interval[find_interval == 0] <- 1
    intervals <- y[find_interval]
    dplyr::mutate(.data, !!estimate := intervals)
}

cal_add_predict_impl <- function(calibration, .data, model) {
  if(calibration$type == "binary") {
    estimate <- names(calibration$estimates)
    model <- calibration$estimates[[estimate]][[model]]$calibration
    preds <- predict(model, newdata = .data, type = "response")
    if(calibration$event_level == 1) preds <- 1 - preds
    .data[[estimate]] <- preds
  }
  .data
}

cal_add_join_impl <- function(calibration, .data, model, desc = NULL) {
  if(calibration$type == "binary") {
    estimate <- calibration$estimates[1]
    ret <- estimate[[1]][[model]]
    cal <- ret$calibration
    est_name <- names(estimate)
    adj_name <- paste0(".adj_", length(colnames(.data)) - 2)
    .data <- cal_add_join(cal, !! parse_expr(est_name), .data, !! parse_expr(est_name))
  }
  .data
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
  round_data <- dplyr::select(
    .data,
    .rounded := round(!!estimate, digits = 3),
    - !! estimate
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

as_cal_object <- function(estimates, truth, type, .data, event_level, additional_class = NULL) {
  lev <- process_level(event_level)
  truth <- enquo(truth)
  truth_name <- as_name(truth)
  levels <- levels(.data[, truth_name][[1]])
  structure(
    list(
      type = type,
      truth = truth_name,
      levels = levels,
      event_level = lev,
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

# ------------------------------ Logistic --------------------------------------

#' @export
cal_logistic <- function(.data,
                         truth = NULL,
                         estimate = NULL,
                         event_level = c("first", "second"),
                         ...
                         ) {
  UseMethod("cal_logistic")
}

#' @export
cal_logistic.data.frame <- function(.data,
                                    truth = NULL,
                                    estimate = NULL,
                                    event_level = c("first", "second"),
                                    ...
                                    ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_model_impl(
      .data = .data,
      truth = !!truth,
      estimate = !!estimate,
      event_level = event_level,
      method = "glm",
      ...
      )
    est <- as_cal_variable(res, !! estimate, "Logistic", "glm")
  } else {
    stop_multiclass()
  }

  as_cal_object(
    estimates = est,
    truth = !!truth,
    type = type,
    .data = .data,
    event_level = event_level,
    additional_class = "cal_logistic"
    )
}

# --------------------- Logistic Spline (GAM)  ---------------------------------

#' @export
cal_logistic_spline <- function(.data,
                                truth = NULL,
                                estimate = NULL,
                                event_level = c("first", "second"),
                                ...
                                ) {
  UseMethod("cal_logistic_spline")
}

#' @export
cal_logistic_spline.data.frame <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           event_level = c("first", "second"),
                                           ...
                                           ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_model_impl(.data, !!truth, !!estimate, method = "logistic_spline", ... = ...)
    est <- as_cal_variable(res, !! estimate, "Logistic Spline", "logistic_spline")
  } else {
    stop_multiclass()
  }

  as_cal_object(
    estimates = est,
    truth = !!truth,
    type = type,
    .data = .data,
    event_level = event_level,
    additional_class = "cal_logistic_spline"
  )
}

# -------------------------------- Beta ----------------------------------------

#' @export
cal_beta <- function(.data, truth = NULL, estimate = NULL, ...) {
  UseMethod("cal_beta")
}

#' @export
cal_beta.data.frame <- function(.data, truth = NULL, estimate = NULL, ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_model_impl(.data, !!truth, !!estimate, method = "beta", ... = ...)
    est <- as_cal_variable(res, !! estimate, "Beta", "beta")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_beta")
}

# ----------------------------- Isotonic ---------------------------------------

#' @export
cal_isotonic <- function(.data, truth = NULL, estimate = NULL, ...) {
  UseMethod("cal_isotonic")
}

#' @export
cal_isotonic.data.frame <- function(.data, truth = NULL, estimate = NULL, ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_dataframe(.data, !!truth, !!estimate, ... = ...)
    est <- as_cal_variable(res, !! estimate, "Isotonic", "isotonic")
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_isotonic")
}

cal_isoreg_dataframe <- function(.data, truth, estimate, truth_val = NULL,
                                 sampled = FALSE, ...) {
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

  model_stepfun <- as.stepfun(model, ... = ...)

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

# ---------------------------- Binary Objs--------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  cat("Probability Calibration\n")
  cat("----------------------- \n")
  cat("Type:      Binary\n")
  cat("Method:    ", x$estimates[[1]][[1]]$title, "\n")
  cat("Truth:     ", x$truth, "\n")
  cat(" |- Levels: ", paste(x$levels, collapse = "/"), "\n")
  cat("Estimate:  ", names(x$estimates), "\n")
}

# ------------------------------- Utils ----------------------------------------

cal_model_impl <- function(.data, truth, estimate, method, event_level, ...) {
  truth <- ensym(truth)
  estimate <- ensym(estimate)

  if(method == "logistic_spline"){
    f_model <- expr(!!truth ~ s(!!estimate, k = 10))
    init_model <- mgcv::gam(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
    }
  if(method == "glm") {
    f_model <- expr(!!truth ~ !!estimate)
    init_model <- glm(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
    }

  model
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
