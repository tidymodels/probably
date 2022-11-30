#------------------------------ cal_apply() ------------------------------------

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

cal_add_adjust.cal_method_logistic <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data
  )
}

cal_add_adjust.cal_method_logistic_spline <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data
  )
}

cal_add_adjust.cal_method_isotonic_boot <- function(calibration, .data, desc = NULL) {
  cal_add_join_impl(calibration, .data, "isotonic_boot", desc = desc)
}

cal_add_adjust.cal_method_isotonic <- function(calibration, .data, desc = NULL) {
  if(calibration$type == "binary") {
    estimate <- calibration$estimates[1]
    estimates_table <- estimate[[1]]
    est_name <- names(estimate)
    adj_name <- paste0(".adj_", length(colnames(.data)) - 2)
    ret <- cal_add_interval(
      estimates_table = estimates_table,
      estimate = !! parse_expr(est_name),
      .data = .data
    )
  }
  ret

}

#---------------------------- Adjust Implementations ---------------------------

cal_add_predict_impl <- function(calibration, .data) {
  if(calibration$type == "binary") {
    model <- calibration$estimates
    preds <- predict(model, newdata = .data, type = "response")
    preds <- 1 - preds
    .data[calibration$levels[[1]]] <- preds
    .data[calibration$levels[[2]]] <- 1 - preds
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

cal_add_interval <- function(estimates_table, estimate, .data) {
  estimate <- enquo(estimate)
  y <- estimates_table$.adj_estimate
  find_interval <- findInterval(
    x = dplyr::pull(.data, !!estimate),
    vec = estimates_table$.estimate
  )
  find_interval[find_interval == 0] <- 1
  intervals <- y[find_interval]
  dplyr::mutate(.data, !!estimate := intervals)
}
