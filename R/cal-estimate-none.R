#--------------------------------- Methods -------------------------------------
#------------------------------- >> None  ----------------------------------
#' Do not calibrate model predictions.
#' @inheritParams cal_estimate_logistic
#' @include cal-estimate-utils.R
#' @param truth The column identifier for the true outcome results
#' (that is factor or numeric). This should be an unquoted column name.
#' @param estimate A vector of column identifiers, or one of `dplyr` selector
#' functions to choose which variables contains the class probabilities or
#' numeric predictions. It defaults to the prefix used by tidymodels (`.pred_`).
#' For classification problems, the order of the identifiers will be considered
#' the same as the order of the levels of the `truth` variable.
#' @details This function does nothing to the predictions. It is used as a
#' reference when tuning over different calibration methods.
#' @examples
#'
#' nada <- cal_estimate_none(boosting_predictions_oob, outcome, .pred)
#' nada
#'
#' identical(
#'   cal_apply(boosting_predictions_oob, nada),
#'   boosting_predictions_oob
#' )
#'
#' # ------------------------------------------------------------------------------
#'
#' nichts <- cal_estimate_none(segment_logistic, Class)
#'
#' identical(
#'   cal_apply(segment_logistic, nichts),
#'   segment_logistic
#' )
#' @export
cal_estimate_none <- function(.data,
                              truth = NULL,
                              estimate = dplyr::starts_with(".pred"),
                              parameters = NULL,
                              ...) {
  UseMethod("cal_estimate_none")
}

#' @export
#' @rdname cal_estimate_none
cal_estimate_none.data.frame <- function(.data,
                                         truth = NULL,
                                         estimate = dplyr::starts_with(".pred"),
                                         parameters = NULL,
                                         ...,
                                         .by = NULL) {
  stop_null_parameters(parameters)
  rlang::check_dots_empty()

  info <- get_prediction_data(.data,
                              truth = {{ truth }},
                              estimate = {{ estimate }},
                              .by = {{ .by }})

  model <- nothing_over_groups(info, ...)

  if (length(info$levels) > 2) {
    cal_type <- "multiclass"
  } else {
    cal_type <- NULL
  }

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "No calibration",
    rows = nrow(info$predictions),
    type = cal_type,
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_none"
  )
}

#' @export
#' @rdname cal_estimate_none
cal_estimate_none.tune_results <- function(.data,
                                           truth = NULL,
                                           estimate = dplyr::starts_with(".pred"),
                                           parameters = NULL,
                                           ...) {
  rlang::check_dots_empty()
  info <- get_tune_data(.data, parameters)

  model <- nothing_over_groups(info, ...)

  if (length(info$levels) > 2) {
    cal_type <- "multiclass"
  } else {
    cal_type <- NULL
  }

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "No calibration",
    rows = nrow(info$predictions),
    type = cal_type,
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_none"
  )
}

#' @export
#' @rdname cal_estimate_none
cal_estimate_none.grouped_df <- function(.data,
                                         truth = NULL,
                                         estimate = NULL,
                                         parameters = NULL,
                                         ...) {
  abort_if_grouped_df()
}

#------------------------------ Implementation ---------------------------------

nothing_over_groups <- function(info,  ...) {
  grp_df <- make_group_df(info$predictions, group = info$group)
  nst_df <- vctrs::vec_split(x = info$predictions, by = grp_df)
  fltrs <- make_cal_filters(nst_df$key)

  # ensemble here

  fits <-
    lapply(
      nst_df$val,
      fit_no_models,
      truth = info$truth,
      estimate = info$estimate,
      ...
    )

  purrr::map2(fits, fltrs, ~ list(filter = .y, estimates = .x))
}

fit_no_models <- function(.data, truth, estimate, ...) {
  res <- list()
  class(res) <- "no_calibration"
  res
}

#' @export
print.no_calibration <- function(x, ...) {
  cli::cli_inform("No calibration")
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_none <- function(x, ...) {
  c("probably")
}

