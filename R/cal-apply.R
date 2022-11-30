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
cal_apply.data.frame <- function(x, calibration, ...) {
  if (calibration$type == "binary") {
    cal_add_adjust(calibration, x)
  } else {
    stop_multiclass()
  }
}

# -------------------------- Adjust Methods ------------------------------------

cal_add_adjust <- function(calibration, .data, desc = NULL) {
  UseMethod("cal_add_adjust")
}

cal_add_adjust.cal_estimate_logistic <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_logistic_spline <- function(calibration, .data, desc = NULL) {
  cal_add_predict_impl(
    calibration = calibration,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_isotonic_boot <- function(calibration, .data, desc = NULL) {
  cal_add_interval_impl(
    calibration = calibration,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_isotonic <- function(calibration, .data, desc = NULL) {
  cal_add_interval_impl(
    calibration = calibration,
    .data = .data
  )
}

#---------------------------- Adjust Implementations ---------------------------

cal_add_predict_impl <- function(calibration, .data) {
  if (calibration$type == "binary") {
    model <- calibration$estimates
    preds <- predict(model, newdata = .data, type = "response")
    preds <- 1 - preds
    .data[calibration$levels[[1]]] <- preds
    .data[calibration$levels[[2]]] <- 1 - preds
  }
  .data
}

cal_add_interval_impl <- function(calibration, .data) {
  if (calibration$type == "binary") {
    estimates_table <- calibration$estimates
    level_1 <- calibration$levels[[1]]
    level_2 <- calibration$levels[[2]]
    intervals <- cal_get_intervals(
      estimates_table = estimates_table,
      .data = .data,
      estimate = level_1
    )
    .data[level_1] <- intervals
    .data[level_2] <- 1 - intervals
  }
  .data
}

cal_get_intervals <- function(estimates_table, .data, estimate) {
  y <- estimates_table$.adj_estimate
  find_interval <- findInterval(
    x = .data[[estimate]],
    vec = estimates_table$.estimate
  )
  find_interval[find_interval == 0] <- 1
  intervals <- y[find_interval]
}
