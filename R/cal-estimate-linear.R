#------------------------------- Methods ---------------------------------------
#' Uses a linear regression model to calibrate numeric predictions
#' @inheritParams cal_estimate_logistic
#' @param .data Am ungrouped  `data.frame` object, or `tune_results` object,
#' that contains a prediction column.
#' @param truth The column identifier for the observed outcome data (that is
#' numeric). This should be an unquoted column name.
#' @param estimate Column identifier for the predicted values
#' @param parameters (Optional)  An optional tibble of tuning parameter values
#' that can be used to filter the predicted values before processing. Applies
#' only to `tune_results` objects.
#' @param ... Additional arguments passed to the models or routines used to
#' calculate the new predictions.
#' @param smooth Applies to the linear models. It switches between a generalized
#' additive model using spline terms when `TRUE`, and simple linear regression
#' when `FALSE`.
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_validate_linear()]
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' head(boosting_predictions_test)
#'
#' # ------------------------------------------------------------------------------
#' # Before calibration
#'
#' y_rng <- extendrange(boosting_predictions_test$outcome)
#'
#' boosting_predictions_test |>
#'   ggplot(aes(outcome, .pred)) +
#'   geom_abline(lty = 2) +
#'   geom_point(alpha = 1 / 2) +
#'   geom_smooth(se = FALSE, col = "blue", linewidth = 1.2, alpha = 3 / 4) +
#'   coord_equal(xlim = y_rng, ylim = y_rng) +
#'   ggtitle("Before calibration")
#'
#' # ------------------------------------------------------------------------------
#' # Smoothed trend removal
#'
#' smoothed_cal <-
#'   boosting_predictions_oob |>
#'   # It will automatically identify the predicted value columns when the
#'   # standard tidymodels naming conventions are used.
#'   cal_estimate_linear(outcome)
#' smoothed_cal
#'
#' boosting_predictions_test |>
#'   cal_apply(smoothed_cal) |>
#'   ggplot(aes(outcome, .pred)) +
#'   geom_abline(lty = 2) +
#'   geom_point(alpha = 1 / 2) +
#'   geom_smooth(se = FALSE, col = "blue", linewidth = 1.2, alpha = 3 / 4) +
#'   coord_equal(xlim = y_rng, ylim = y_rng) +
#'   ggtitle("After calibration")
#'
#' @details
#' This function uses existing modeling functions from other packages to create
#' the calibration:
#'
#' - [stats::glm()] is used when `smooth` is set to `FALSE`
#' - [mgcv::gam()] is used when `smooth` is set to `TRUE`
#'
#' These methods estimate the relationship in the unmodified predicted values
#' and then remove that trend when [cal_apply()] is invoked.
#' @export
cal_estimate_linear <- function(.data,
                                truth = NULL,
                                estimate = dplyr::matches("^.pred$"),
                                smooth = TRUE,
                                parameters = NULL,
                                ...,
                                .by = NULL) {
  UseMethod("cal_estimate_linear")
}

#' @export
#' @rdname cal_estimate_linear
cal_estimate_linear.data.frame <- function(.data,
                                           truth = NULL,
                                           estimate = dplyr::matches("^.pred$"),
                                           smooth = TRUE,
                                           parameters = NULL,
                                           ...,
                                           .by = NULL) {
  stop_null_parameters(parameters)

  info <- get_prediction_data(
    .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    .by = {{ .by }}
  )

  model_fit <- lin_reg_fit_over_groups(info, smooth, ...)

  if (smooth) {
    model <- "linear_spline"
    method <- "Generalized additive model calibration"
    additional_class <- "cal_estimate_linear_spline"
  } else {
    model <- "glm"
    method <- "Linear calibration"
    additional_class <- "cal_estimate_linear"
  }

  as_regression_cal_object(
    estimate = model_fit,
    levels = info$map,
    truth = info$truth,
    method = method,
    rows = nrow(.data),
    additional_class = additional_class,
    source_class = cal_class_name(.data)
  )

}

#' @export
#' @rdname cal_estimate_linear
cal_estimate_linear.tune_results <- function(.data,
                                             truth = NULL,
                                             estimate = dplyr::matches("^.pred$"),
                                             smooth = TRUE,
                                             parameters = NULL,
                                             ...) {
  info <- get_tune_data(.data, parameters)

  model_fit <- lin_reg_fit_over_groups(info, smooth, ...)

  if (smooth) {
    model <- "linear_spline"
    method <- "Generalized additive model calibration"
    additional_class <- "cal_estimate_linear_spline"
  } else {
    model <- "glm"
    method <- "Linear calibration"
    additional_class <- "cal_estimate_linear"
  }

  as_regression_cal_object(
    estimate = model_fit,
    levels = info$map,
    truth = info$truth,
    method = method,
    rows = nrow(info$predictions),
    additional_class = additional_class,
    source_class = cal_class_name(.data)
  )
}

#' @export
#' @rdname cal_estimate_linear
cal_estimate_linear.grouped_df <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           smooth = TRUE,
                                           parameters = NULL,
                                           ...) {
  abort_if_grouped_df()
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_linear_spline <- function(x, ...) {
  check_req_pkgs(x)
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_linear <- function(x, ...) {
  c("probably")
}

#--------------------------- Implementation ------------------------------------

fit_regression_model <- function(.data, smooth, estimate, outcome, ...) {
  smooth <- turn_off_smooth_if_too_few_unique(.data, estimate, smooth)

  if (smooth) {
    f_model <- rlang::expr(!!rlang::sym(outcome) ~ s(!!rlang::sym(estimate)))
    f_model <- stats::as.formula(f_model)
    init_model <- mgcv::gam(f_model, data = .data, ...)
    model <- butcher::butcher(init_model)
  } else {
    f_model <- rlang::expr(!!rlang::sym(outcome) ~ !!rlang::sym(estimate))
    f_model <- stats::as.formula(f_model)
    init_model <- stats::glm(f_model, data = .data, ...)
    model <- butcher::butcher(init_model)
  }

  model
}


lin_reg_fit_over_groups <- function(info, smooth = TRUE, ...) {
  if (length(info$levels) == 2) {
    cli::cli_abort("This function is meant to be used with multi-class outcomes only.")
  }

  grp_df <- make_group_df(info$predictions, group = info$group)
  nst_df <- vctrs::vec_split(x = info$predictions, by = grp_df)
  fltrs <- make_cal_filters(nst_df$key)

  fits <-
    lapply(
      nst_df$val,
      fit_regression_model,
      smooth = smooth,
      estimate = info$estimate,
      info$truth,
      ...
    )

  purrr::map2(fits, fltrs, ~ list(filter = .y, estimate = .x))
}


#' @export
print.cal_estimate_linear <- function(x, ...) {
  print_reg_cal(x, upv = FALSE, ...)
}
