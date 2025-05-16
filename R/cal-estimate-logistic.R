#------------------------------- Methods ---------------------------------------
#' Uses a logistic regression model to calibrate probabilities
#' @param .data An ungrouped `data.frame` object, or `tune_results` object,
#' that contains predictions and probability columns.
#' @param truth The column identifier for the true class results
#' (that is a factor). This should be an unquoted column name.
#' @param estimate A vector of column identifiers, or one of `dplyr` selector
#' functions to choose which variables contains the class probabilities. It
#' defaults to the prefix used by tidymodels (`.pred_`). The order of the
#' identifiers will be considered the same as the order of the levels of the
#' `truth` variable.
#' @param parameters (Optional)  An optional tibble of tuning parameter values
#' that can be used to filter the predicted values before processing. Applies
#' only to `tune_results` objects.
#' @param .by The column identifier for the grouping variable. This should be
#' a single unquoted column name that selects a qualitative variable for
#' grouping. Default to `NULL`. When `.by = NULL` no grouping will take place.
#' @param ... Additional arguments passed to the models or routines used to
#' calculate the new probabilities.
#' @param smooth Applies to the logistic models. It switches between logistic
#' spline when `TRUE`, and simple logistic regression when `FALSE`.
#' @examples
#' # It will automatically identify the probability columns
#' # if passed a model fitted with tidymodels
#' cal_estimate_logistic(segment_logistic, Class)
#'
#' # Specify the variable names in a vector of unquoted names
#' cal_estimate_logistic(segment_logistic, Class, c(.pred_poor, .pred_good))
#'
#' # dplyr selector functions are also supported
#' cal_estimate_logistic(segment_logistic, Class, dplyr::starts_with(".pred_"))
#' @details
#' This function uses existing modeling functions from other packages to create
#' the calibration:
#' - [stats::glm()] is used when `smooth` is set to `FALSE`
#' - [mgcv::gam()] is used when `smooth` is set to `TRUE`
#'
#' ## Multiclass Extension
#'
#' This method has _not_ been extended to multiclass outcomes. However, the
#' natural multiclass extension is [cal_estimate_multinomial()].
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_validate_logistic()]
#' @export
cal_estimate_logistic <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred_"),
  smooth = TRUE,
  parameters = NULL,
  ...
) {
  UseMethod("cal_estimate_logistic")
}

#' @export
#' @rdname cal_estimate_logistic
cal_estimate_logistic.data.frame <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred_"),
  smooth = TRUE,
  parameters = NULL,
  ...,
  .by = NULL
) {
  stop_null_parameters(parameters)

  info <- get_prediction_data(
    .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    .by = {{ .by }}
  )

  model <- logistic_fit_over_groups(info, smooth, ...)

  if (smooth) {
    method <- "Generalized additive model calibration"
    additional_class <- "cal_estimate_logistic_spline"
  } else {
    method <- "Logistic regression calibration"
    additional_class <- "cal_estimate_logistic"
  }

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = method,
    rows = nrow(info$predictions),
    additional_classes = additional_class,
    source_class = cal_class_name(.data),
    type = "binary"
  )

}

#' @export
#' @rdname cal_estimate_logistic
cal_estimate_logistic.tune_results <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred_"),
  smooth = TRUE,
  parameters = NULL,
  ...
) {

  info <- get_tune_data(.data, parameters)

  model <- logistic_fit_over_groups(info, smooth, ...)

  if (smooth) {
    method <- "Generalized additive model calibration"
    additional_class <- "cal_estimate_logistic_spline"
  } else {
    method <- "Logistic regression calibration"
    additional_class <- "cal_estimate_logistic"
  }

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = method,
    rows = nrow(info$predictions),
    additional_classes = additional_class,
    source_class = cal_class_name(.data),
    type = "binary"
  )
}

#' @export
#' @rdname cal_estimate_logistic
cal_estimate_logistic.grouped_df <- function(
  .data,
  truth = NULL,
  estimate = NULL,
  smooth = TRUE,
  parameters = NULL,
  ...
) {
  abort_if_grouped_df()
}


#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_logistic_spline <- function(x, ...) {
  check_req_pkgs(x)
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_logistic <- function(x, ...) {
  c("probably")
}

#--------------------------- Implementation ------------------------------------

fit_logistic_model <- function(.data, smooth, estimate, outcome, ...) {
  smooth <- turn_off_smooth_if_too_few_unique(.data, estimate, smooth)

  f <- f_from_str(outcome, estimate[-length(estimate)], smooth)
  if (smooth) {
    # TODO check for failures
    model <- mgcv::gam(f, data = .data, family = "binomial", ...)
  } else {
    # TODO check for failures
    model <- glm(f, data = .data, family = "binomial", ...)

}
  model <- butcher::butcher(model)
  model
  }

logistic_fit_over_groups <- function(info, smooth = TRUE, ...) {
  if (length(info$levels) > 2) {
    cli::cli_abort(
      "The {.arg truth} column has {length(info$levels)} levels ({.val {info$levels}}),
       but only two-class factors are allowed for this calibration method."
    )
  }

  grp_df <- make_group_df(info$predictions, group = info$group)
  nst_df <- vctrs::vec_split(x = info$predictions, by = grp_df)
  fltrs <- make_cal_filters(nst_df$key)

  fits <-
    lapply(
      nst_df$val,
      fit_logistic_model,
      smooth = smooth,
      estimate = info$estimate,
      info$truth,
      ...
    )

  purrr::map2(fits, fltrs, ~ list(filter = .y, estimate = .x))
}

