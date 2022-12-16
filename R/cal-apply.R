#---------------------------------- Apply --------------------------------------

#' Applies a calibration to a set of prediction probabilities
#' @details It currently supports data.frames only. It extracts the `truth` and
#' the estimate columns names, and levels, from the calibration object.
#' @param .data An object that can process a calibration object.
#' @param object The calibration object (`cal_object`).
#' @param prediction (Optional) Column identifier with the prediction.
#' @param threshold (Optional) Applies to binary models only. A number between
#' 0 and 1. It contains the cut off for the prediction
#' @param ... Optional arguments; currently unused.
#' @examples
#' w_calibration <- cal_estimate_logistic(segment_logistic, Class)
#'
#' cal_apply(segment_logistic, w_calibration)
#' @export
cal_apply <- function(.data, object, prediction = NULL, threshold = NULL, ...) {
  rlang::check_dots_empty()
  UseMethod("cal_apply")
}

#' @export
cal_apply.data.frame <- function(.data, object, prediction = NULL, threshold = NULL, ...) {
  if (object$type == "binary") {
    cal_add_adjust(
      object = object,
      .data = .data,
      prediction = {{prediction}},
      threshold = threshold
      )
  } else {
    stop_multiclass()
  }
}

#' @export
cal_apply.cal_object <- function(.data, object, prediction = NULL, threshold = NULL, ...) {
  rlang::abort(paste0("`cal_apply()` expects the data as the first argument,",
                 "and the object object as the second argument."
                 ))
}

# ------------------------------- Adjust ---------------------------------------

cal_add_adjust <- function(object, .data, prediction, threshold) {
  UseMethod("cal_add_adjust")
}

cal_add_adjust.cal_estimate_logistic <- function(object,
                                                 .data,
                                                 prediction = NULL,
                                                 threshold = NULL,
                                                 ...) {
  prediction <- enquo(prediction)

  new_data <- cal_add_predict_impl(
    object = object,
    .data = .data
  )

  if(!quo_is_null(prediction)) {
    if(object$type == "binary") {
      level1_gt <- new_data[[object$levels[[1]]]] > new_data[[object$levels[[2]]]]
      new_data[level1_gt, as_name(!! prediction)] <- names(object$levels[1])
      new_data[!level1_gt, as_name(!! prediction)] <- names(object$levels[2])
    }
  }

  new_data
}

cal_add_adjust.cal_estimate_logistic_spline <- function(object,
                                                        .data,
                                                        prediction = NULL,
                                                        threshold = NULL,
                                                        ...
                                                        ) {
  prediction <- enquo(prediction)

  new_data <- cal_add_predict_impl(
    object = object,
    .data = .data
  )

  if(!quo_is_null(prediction)) {
    if(object$type == "binary") {
      pred_name <- as_name(prediction)
      level1_gt <- new_data[[object$levels[[1]]]] > new_data[[object$levels[[2]]]]
      new_data[level1_gt, pred_name] <- names(object$levels[1])
      new_data[!level1_gt, pred_name] <- names(object$levels[2])
    }
  }

  new_data
}

cal_add_adjust.cal_estimate_isotonic_boot <- function(object,
                                                      .data,
                                                      prediction = NULL,
                                                      threshold = NULL,
                                                      ...
                                                      ) {
  cal_add_interval_impl(
    object = object,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_isotonic <- function(object,
                                                 .data,
                                                 prediction = NULL,
                                                 threshold = NULL,
                                                 ...
                                                 ) {
  cal_add_interval_impl(
    object = object,
    .data = .data
  )
}

#---------------------------- Adjust implementations ---------------------------

cal_add_predict_impl <- function(object, .data) {
  if (object$type == "binary") {
    .data <- object$estimates %>%
      purrr::map(
        ~ {
          if(is.null(.x$filter)) {
            new_data <- .data
          } else {
            new_data <- dplyr::filter(.data, !! .x$filter)
          }
          preds <- predict(.x$estimate, newdata = new_data, type = "response")
          preds <- 1 - preds
          new_data[object$levels[[1]]] <- preds
          new_data[object$levels[[2]]] <- 1 - preds
          new_data
        }) %>%
      purrr::reduce(dplyr::bind_rows)
  }
  .data
}

cal_add_interval_impl <- function(object, .data) {
  if (object$type == "binary") {
    estimates_table <- object$estimates
    level_1 <- object$levels[[1]]
    level_2 <- object$levels[[2]]
    if("data.frame" %in% class(estimates_table)) {
      intervals <- cal_get_intervals(
        estimates_table = estimates_table,
        .data = .data,
        estimate = level_1
      )
    } else {
      intervals <- estimates_table %>%
        map(cal_get_intervals, .data, level_1) %>%
        unlist() %>%
        matrix(nrow = nrow(.data)) %>%
        apply(1, mean)
    }
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
  y[find_interval]
}
