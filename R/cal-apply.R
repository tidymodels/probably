#---------------------------------- Apply --------------------------------------

#' Applies a calibration to a set of prediction probabilities
#' @details It currently supports data.frames only. It extracts the `truth` and
#' the estimate columns names, and levels, from the calibration object.
#' @param .data An object that can process a calibration object.
#' @param object The calibration object (`cal_object`).
#' @param ... Optional arguments; currently unused.
#' @examples
#' w_calibration <- cal_estimate_logistic(segment_logistic, Class)
#'
#' cal_apply(segment_logistic, w_calibration)
#' @export
cal_apply <- function(.data, object, ...) {
  rlang::check_dots_empty()
  UseMethod("cal_apply")
}

#' @export
cal_apply.data.frame <- function(.data, object, ...) {
  if (object$type == "binary") {
    cal_add_adjust(object, .data)
  } else {
    stop_multiclass()
  }
}

#' @export
cal_apply.cal_object <- function(.data, object, ...) {
  rlang::abort(paste0("`cal_apply()` expects the data as the first argument,",
                 "and the object object as the second argument."
                 ))
}

# ------------------------------- Adjust ---------------------------------------

cal_add_adjust <- function(object, .data) {
  UseMethod("cal_add_adjust")
}

cal_add_adjust.cal_estimate_logistic <- function(object, .data) {
  cal_add_predict_impl(
    object = object,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_logistic_spline <- function(object, .data) {
  cal_add_predict_impl(
    object = object,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_isotonic_boot <- function(object, .data) {
  cal_add_interval_impl(
    object = object,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_isotonic <- function(object, .data) {
  cal_add_interval_impl(
    object = object,
    .data = .data
  )
}

#---------------------------- Adjust implementations ---------------------------

cal_add_predict_impl <- function(object, .data) {
  if (object$type == "binary") {
    model <- object$estimates
    preds <- predict(model, newdata = .data, type = "response")
    preds <- 1 - preds
    .data[object$levels[[1]]] <- preds
    .data[object$levels[[2]]] <- 1 - preds
  }
  .data
}

cal_add_interval_impl <- function(object, .data) {
  if (object$type == "binary") {
    estimates_table <- object$estimates
    level_1 <- object$levels[[1]]
    level_2 <- object$levels[[2]]
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
