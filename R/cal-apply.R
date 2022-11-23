#' Applies a calibration to a set of prediction probabilities
#' @details It currently supports data.frames only. It extracts the `truth` and
#' the estimate columns names, and levels, from the calibration object.
#' @param x An object that can process a calibration object.
#' @param calibration The calibration object (`cal_object`).
#' @param ... Optional arguments; currently unused.
#' @export
cal_apply <- function(x, calibration, ...) {
  UseMethod("cal_apply")
}

#' @export
cal_apply.data.frame <- function(x, calibration, ...){
  if(calibration$type == "binary") {
    cal_add_adjust(calibration, x)
  } else {
    stop_multiclass()
  }
}

# -------------------------- Adjust Methods ------------------------------------

cal_add_adjust <- function(calibration, .data, desc = NULL) {
  UseMethod("cal_add_adjust")
}

cal_add_adjust.cal_logistic <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data
  )
}

cal_add_adjust.cal_logistic_spline <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data
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

#---------------------------- Adjust Implementations ---------------------------

cal_add_predict_impl <- function(calibration, .data) {
  if(calibration$type == "binary") {
    estimate <- names(calibration$estimates)
    model <- calibration$estimates[[estimate]]
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