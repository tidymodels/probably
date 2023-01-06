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
                      ...) {
  rlang::check_dots_empty()
  UseMethod("cal_apply")
}

#' @export
#' @rdname cal_apply
cal_apply.data.frame <- function(.data,
                                 object,
                                 pred_class = NULL,
                                 parameters = NULL,
                                 ...) {
  stop_null_parameters(parameters)

  if (object$type == "binary") {
    res <- cal_adjust_binary(
      object = object,
      .data = .data,
      pred_class = {{ pred_class }}
    )
  }

  if(object$type == "multiclass") {
    res <- cal_adjust_multi(
      object = object,
      .data = .data
    )
  }

  cal_update_prediction(
    .data = res,
    object = object,
    pred_class = {{ pred_class }}
  )
}

#' @export
#' @rdname cal_apply
cal_apply.tune_results <- function(.data,
                                   object,
                                   pred_class = NULL,
                                   parameters = NULL,
                                   ...) {

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

    predictions <- tune::collect_predictions(
      x = .data,
      summarize = TRUE,
      parameters = parameters,
      ...
    )

    if (object$type == "binary") {
      res <- cal_adjust_binary(
        object = object,
        .data = predictions,
        pred_class = !!pred_class
        )
    }

    if(object$type == "multiclass") {
      res <- cal_adjust_multi(
        object = object,
        .data = predictions,
        pred_class = !!pred_class
      )
    }

    res
}

#' @export
#' @rdname cal_apply
cal_apply.cal_object <- function(.data,
                                 object,
                                 pred_class = NULL,
                                 parameters = NULL,
                                 ...) {
  if ("data.frame" %in% class(object)) {
    rlang::abort(paste0(
      "`cal_apply()` expects the data as the first argument,",
      " and the object as the second argument. Please reverse",
      " the order of the arguments and try again."
    ))
  }
}

cal_update_prediction <- function(.data, object, pred_class) {
  res <- .data
  if (!is.null(pred_class)) {

    pred_name <- as_name(pred_class)
    if (pred_name %in% colnames(.data)) {
      .data[, pred_name] <- NULL
    }

    if (object$type == "binary") {
      level1_gt <- res[[object$levels[[1]]]] > res[[object$levels[[2]]]]
      res[level1_gt, pred_name] <- names(object$levels[1])
      res[!level1_gt, pred_name] <- names(object$levels[2])
      res[, pred_name] <- as.factor(res[, pred_name][[1]])
    }

    if (object$type == "multiclass") {
      max_cols <- max.col(res[, as.character(object$levels)])
      factor_cols <- as.factor(max_cols)
      levels(factor_cols) <-names(object$levels)
      res[, pred_name] <- factor_cols
    }
  }
  res
}
