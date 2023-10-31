#---------------------------------- Methods ------------------------------------

# TODO for regression; update a lot of help pages to talk more generally about multiple types

#' Applies a calibration to a set of existing predictions
#' @details
#'
#' `cal_apply()` currently supports data.frames only. It extracts the `truth` and
#' the estimate columns names from the calibration object.
#'
#' @param .data An object that can process a calibration object.
#' @param object The calibration object (`cal_object`).
#' @param pred_class (Optional, classification only) Column identifier for the
#' hard class predictions (a factor vector). This column will be adjusted based
#' on changes to the calibrated probability columns.
#' @param parameters (Optional)  An optional tibble of tuning parameter values
#' that can be used to filter the predicted values before processing. Applies
#' only to `tune_results` objects.
#' @param ... Optional arguments; currently unused.
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_estimate_beta()], [cal_estimate_isotonic()],
#' [cal_estimate_isotonic_boot()], [cal_estimate_linear()],
#' [cal_estimate_logistic()], [cal_estimate_multinomial()]
#' @examples
#'
#' # ------------------------------------------------------------------------------
#' # classification example
#'
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
  cal_pkg_check(required_pkgs(object))

  stop_null_parameters(parameters)

  cal_adjust_update(
    object = object,
    .data = .data,
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
  cal_pkg_check(required_pkgs(object))

  if (!(".predictions" %in% colnames(.data))) {
    rlang::abort(
      paste0(
        "The `tune_results` object does not contain columns with predictions",
        " Refit with the control argument `save_pred = TRUE` to save these columns."
      )
    )
  }

  pred_class <- enquo(pred_class)

  if (rlang::quo_is_null(pred_class)) {
    pred_class <- rlang::parse_expr(".pred_class")
  }

  cp <- tune::collect_predictions(
    x = .data,
    summarize = TRUE,
    parameters = parameters,
    ...
  )

  cal_adjust_update(
    object = object,
    .data = cp,
    pred_class = !!pred_class
  )
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

#---------------------------------- Adjust -------------------------------------

cal_adjust <- function(object, .data, pred_class) {
  UseMethod("cal_adjust")
}

cal_adjust.cal_estimate_isotonic <- function(object, .data, pred_class) {
  apply_interval_impl(
    object = object,
    .data = .data,
    multi = FALSE
  )
}

cal_adjust.cal_estimate_isotonic_boot <- function(object, .data, pred_class) {
  apply_interval_impl(
    object = object,
    .data = .data,
    multi = TRUE
  )
}

cal_adjust.cal_estimate_beta <- function(object,
                                         .data,
                                         pred_class = NULL,
                                         ...) {
  apply_beta_impl(
    object = object,
    .data = .data
  )
}

cal_adjust.cal_multi <- function(object, .data, pred_class) {
  cal_apply_multi(
    object = object,
    .data = .data,
    pred_class = {{ pred_class }}
  )
}

cal_adjust.cal_binary <- function(object, .data, pred_class) {
  cal_apply_binary(
    object = object,
    .data = .data,
    pred_class = {{ pred_class }}
  )
}

cal_adjust.cal_regression <- function(object, .data, pred_class) {
  cal_apply_regression(
    object = object,
    .data = .data,
    pred_class = NULL
  )
}

cal_adjust_update <- function(.data,
                              object,
                              pred_class = NULL,
                              parameters = NULL,
                              ...) {
  if (object$type != "regression") {
    pred_class <- enquo(pred_class)
  } else {
    pred_class <- quo(NULL)
  }

  res <- cal_adjust(
    object = object,
    .data = .data,
    pred_class = !!pred_class
  )

  if (!rlang::quo_is_null(pred_class)) {
    pred_name <- as_name(pred_class)

    if (pred_name %in% colnames(res)) {
      res[, pred_name] <- NULL
    }

    col_names <- as.character(object$levels)
    factor_levels <- names(object$levels)

    predictions <- res[, col_names] %>%
      max.col(ties.method = "first") %>%
      factor_levels[.] %>%
      factor(levels = factor_levels)

    res[, pred_name] <- predictions
  }
  res
}
