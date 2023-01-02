#---------------------------------- Apply --------------------------------------

#' Applies a calibration to a set of pred_class probabilities
#' @details It currently supports data.frames only. It extracts the `truth` and
#' the estimate columns names, and levels, from the calibration object.
#' @param .data An object that can process a calibration object.
#' @param object The calibration object (`cal_object`).
#' @param pred_class (Optional) Column identifier for the hard class predictions
#' (a factor vector). This column will be adjusted based on changes to the
#' calibrated probability columns.
#' @param parameters (Optional)  An optional tibble of tuning parameter values
#' that can be used to filter the predicted values before processing. Applies
#' only to `tune_results` objects.
#' @param ... Optional arguments; currently unused.
#' @examples
#' w_calibration <- cal_estimate_logistic(segment_logistic, Class)
#'
#' cal_apply(segment_logistic, w_calibration)
#' @export
cal_apply <- function(.data,
                      object,
                      pred_class = NULL,
                      parameters = NULL,
                      ...
                      ) {
  rlang::check_dots_empty()
  UseMethod("cal_apply")
}

#' @export
#' @rdname cal_apply
cal_apply.data.frame <- function(.data,
                                 object,
                                 pred_class = NULL,
                                 parameters = NULL,
                                 ...
                                 ) {
  stop_null_parameters(parameters)
  if (object$type == "binary") {
    cal_add_adjust(
      object = object,
      .data = .data,
      pred_class = {{ pred_class }}
    )
  } else {
    stop_multiclass()
  }
}

#' @export
#' @rdname cal_apply
cal_apply.tune_results <- function(.data,
                                   object,
                                   pred_class = NULL,
                                   parameters = NULL,
                                   ...
                                   ) {
  if (object$type == "binary") {
    if (!(".predictions" %in% colnames(.data))) {
      rlang::abort(
        paste0(
          "The `tune_results` object does not contain the `.predictions` column.",
          " Refit with the control argument `save_pred = TRUE` to save pred_classs."
        )
      )
    }

    pred_class <- enquo(pred_class)

    if (rlang::quo_is_null(pred_class)) {
      pred_class <- rlang::parse_expr(".pred_class")
    }

    pred_classs <- tune::collect_predictions(
      x = .data,
      summarize = TRUE,
      parameters = parameters,
      ...
      )

    cal_add_adjust(
      object = object,
      .data = pred_classs,
      pred_class = !!pred_class
    )
  } else {
    stop_multiclass()
  }
}

#' @export
#' @rdname cal_apply
cal_apply.cal_object <- function(.data,
                                 object,
                                 pred_class = NULL,
                                 parameters = NULL,
                                 ...
                                 ) {
  if ("data.frame" %in% class(object)) {
    rlang::abort(paste0(
      "`cal_apply()` expects the data as the first argument,",
      " and the object as the second argument. Please reverse",
      " the order of the arguments and try again."
    ))
  }
}

# ------------------------------- Adjust ---------------------------------------

cal_add_adjust <- function(object, .data, pred_class) {
  UseMethod("cal_add_adjust")
}

cal_add_adjust.cal_estimate_logistic <- function(object,
                                                 .data,
                                                 pred_class = NULL,
                                                 ...) {
  pred_class <- enquo(pred_class)

  new_data <- cal_add_predict_impl(
    object = object,
    .data = .data
  )

  if (!quo_is_null(pred_class)) {
    if (object$type == "binary") {
      level1_gt <- new_data[[object$levels[[1]]]] > new_data[[object$levels[[2]]]]
      new_data[level1_gt, as_name(pred_class)] <- names(object$levels[1])
      new_data[!level1_gt, as_name(pred_class)] <- names(object$levels[2])
    }
  }

  new_data
}

cal_add_adjust.cal_estimate_logistic_spline <- function(object,
                                                        .data,
                                                        pred_class = NULL,
                                                        ...) {
  pred_class <- enquo(pred_class)

  new_data <- cal_add_predict_impl(
    object = object,
    .data = .data
  )

  if (!quo_is_null(pred_class)) {
    if (object$type == "binary") {
      pred_name <- as_name(pred_class)
      level1_gt <- new_data[[object$levels[[1]]]] > new_data[[object$levels[[2]]]]
      new_data[level1_gt, pred_name] <- names(object$levels[1])
      new_data[!level1_gt, pred_name] <- names(object$levels[2])
    }
  }

  new_data
}

cal_add_adjust.cal_estimate_isotonic_boot <- function(object,
                                                      .data,
                                                      pred_class = NULL,
                                                      ...) {
  cal_add_interval_impl(
    object = object,
    .data = .data,
    multi = TRUE
  )
}

cal_add_adjust.cal_estimate_isotonic <- function(object,
                                                 .data,
                                                 pred_class = NULL,
                                                 ...) {
  cal_add_interval_impl(
    object = object,
    .data = .data
  )
}

cal_add_adjust.cal_estimate_beta <- function(object,
                                             .data,
                                             pred_class = NULL,
                                             ...) {
  if (object$type == "binary") {
    p <- dplyr::pull(.data, !!object$levels[[1]])
    model <- object$estimates[[1]]$estimate
    preds <- betacal::beta_predict(
      p = p,
      calib = model
    )
    .data[object$levels[[1]]] <- preds
    .data[object$levels[[2]]] <- 1 - preds
  }
  .data
}

#---------------------------- Adjust implementations ---------------------------

cal_add_predict_impl <- function(object, .data) {
  if (object$type == "binary") {
    .data <- object$estimates %>%
      purrr::map(
        ~ {
          if (is.null(.x$filter)) {
            new_data <- .data
          } else {
            new_data <- dplyr::filter(.data, !!.x$filter)
          }
          preds <- predict(.x$estimate, newdata = new_data, type = "response")
          preds <- 1 - preds
          new_data[object$levels[[1]]] <- preds
          new_data[object$levels[[2]]] <- 1 - preds
          new_data
        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)
  }
  .data
}

cal_add_interval_impl <- function(object, .data, multi = FALSE) {
  if (object$type == "binary") {
    level_1 <- object$levels[[1]]
    level_2 <- object$levels[[2]]

    .data <- object$estimates %>%
      purrr::map(
        ~ {
          if (is.null(.x$filter)) {
            new_data <- .data
          } else {
            new_data <- dplyr::filter(.data, !!.x$filter)
          }

          if (!multi) {
            intervals <- cal_get_intervals(
              estimates_table = .x$estimates[[1]],
              .data = new_data,
              estimate = level_1
            )
          } else {
            intervals <- .x$estimates %>%
              map(cal_get_intervals,
                .data = new_data,
                estimate = level_1
              ) %>%
              unlist() %>%
              matrix(nrow = nrow(new_data)) %>%
              apply(1, mean)
          }
          new_data[level_1] <- intervals
          new_data[level_2] <- 1 - intervals
          new_data
        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)
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
