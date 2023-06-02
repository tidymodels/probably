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
cal_estimate_logistic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  smooth = TRUE,
                                  parameters = NULL,
                                  ...) {
  UseMethod("cal_estimate_logistic")
}

#' @export
#' @rdname cal_estimate_logistic
cal_estimate_logistic.data.frame <- function(.data,
                                             truth = NULL,
                                             estimate = dplyr::starts_with(".pred_"),
                                             smooth = TRUE,
                                             parameters = NULL,
                                             ...,
                                             .by = NULL) {
  stop_null_parameters(parameters)

  group <- get_group_argument({{ .by }}, .data)
  .data <- dplyr::group_by(.data, dplyr::across({{ group }}))

  cal_logistic_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    smooth = smooth,
    source_class = cal_class_name(.data),
    ...
  )
}

#' @export
#' @rdname cal_estimate_logistic
cal_estimate_logistic.tune_results <- function(.data,
                                               truth = NULL,
                                               estimate = dplyr::starts_with(".pred_"),
                                               smooth = TRUE,
                                               parameters = NULL,
                                               ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    event_level = "first",
    parameters = parameters,
    ...
  )

  tune_args$predictions %>%
    dplyr::group_by(!!tune_args$group) %>%
    cal_logistic_impl(
      truth = !!tune_args$truth,
      estimate = !!tune_args$estimate,
      smooth = smooth,
      source_class = cal_class_name(.data),
      ...
    )
}

#' @export
#' @rdname cal_estimate_logistic
cal_estimate_logistic.grouped_df <- function(.data,
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
required_pkgs.cal_estimate_logistic_spline <- function(x, ...) {
  c("mgcv", "probably")
}


#--------------------------- Implementation ------------------------------------
cal_logistic_impl <- function(.data,
                              truth = NULL,
                              estimate = dplyr::starts_with(".pred_"),
                              type,
                              smooth,
                              source_class = NULL,
                              ...) {
  if (smooth) {
    model <- "logistic_spline"
    method <- "Generalized additive model"
    additional_class <- "cal_estimate_logistic_spline"
  } else {
    model <- "glm"
    method <- "Logistic regression"
    additional_class <- "cal_estimate_logistic"
  }

  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }}, validate = TRUE)

  if (length(levels) == 2) {
    log_model <- cal_logistic_impl_grp(
      .data = .data,
      truth = !!truth,
      estimate = levels[[1]],
      run_model = model,
      ...
    )

    res <- as_cal_object(
      estimate = log_model,
      levels = levels,
      truth = !!truth,
      method = method,
      rows = nrow(.data),
      additional_classes = additional_class,
      source_class = source_class
    )
  } else {
    msg <- paste("The number of outcome factor levels isn't consistent with",
                 "the calibration method. Only two class `truth` factors are",
                 "allowed. The given levels were:",
                 paste0("'", levels, "'", collapse = ", "))
    rlang::abort(msg)
  }

  res
}

cal_logistic_impl_grp <- function(.data, truth, estimate, run_model, group, ...) {
  .data %>%
    dplyr::group_by({{ group }}, .add = TRUE) %>%
    split_dplyr_groups() %>%
    lapply(
      function(x) {
        estimate <- cal_logistic_impl_single(
          .data = x$data,
          truth = {{ truth }},
          estimate = estimate,
          run_model = run_model,
          ... = ...
        )
        list(
          filter = x$filter,
          estimate = estimate
        )
      }
    )
}

cal_logistic_impl_single <- function(.data, truth, estimate, run_model, ...) {
  truth <- ensym(truth)

  if (run_model == "logistic_spline") {
    f_model <- expr(!!truth ~ s(!!estimate))
    init_model <- mgcv::gam(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
  }

  if (run_model == "glm") {
    f_model <- expr(!!truth ~ !!estimate)
    init_model <- glm(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
  }

  model
}
