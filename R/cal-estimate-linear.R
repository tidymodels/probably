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
#' boosting_predictions_test %>%
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
#'   boosting_predictions_oob %>%
#'   # It will automatically identify the predicted value columns when the
#'   # standard tidymodels naming conventions are used.
#'   cal_estimate_linear(outcome)
#' smoothed_cal
#'
#' boosting_predictions_test %>%
#'   cal_apply(smoothed_cal) %>%
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

  group <- get_group_argument({{ .by }}, .data)
  .data <- dplyr::group_by(.data, dplyr::across({{ group }}))

  cal_linear_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    smooth = smooth,
    source_class = cal_class_name(.data),
    ...
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
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    event_level = NA_character_,
    parameters = parameters,
    ...
  )

  tune_args$predictions %>%
    dplyr::group_by(!!tune_args$group) %>%
    cal_linear_impl(
      truth = !!tune_args$truth,
      estimate = !!tune_args$estimate,
      smooth = smooth,
      source_class = cal_class_name(.data),
      ...
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
  c("mgcv", "probably")
}


#--------------------------- Implementation ------------------------------------
cal_linear_impl <- function(.data,
                            truth = NULL,
                            estimate = dplyr::starts_with(".pred"),
                            type,
                            smooth,
                            source_class = NULL,
                            ...) {
  if (smooth) {
    model <- "linear_spline"
    method <- "Generalized additive model"
    additional_class <- "cal_estimate_linear_spline"
  } else {
    model <- "glm"
    method <- "Linear"
    additional_class <- "cal_estimate_linear"
  }

  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }})

  if (length(levels) == 1) {

    # check outcome type:
    y <- rlang::eval_tidy(levels[[1]], .data)
    if (!is.vector(y) || !is.numeric(y) || is.factor(y)) {
      rlang::abort("Predictions should be a single numeric vector.")
    }

    lin_model <- cal_linear_impl_grp(
      .data = .data,
      truth = !!truth,
      estimate = levels[[1]],
      run_model = model,
      ...
    )

    res <- as_regression_cal_object(
      estimate = lin_model,
      levels = levels,
      truth = !!truth,
      method = method,
      rows = nrow(.data),
      additional_class = additional_class,
      source_class = source_class
    )
  } else {
    rlang::abort("Outcome data should be a single numeric vector.")
  }

  res
}

cal_linear_impl_grp <- function(.data, truth, estimate, run_model, group, ...) {
  .data %>%
    dplyr::group_by({{ group }}, .add = TRUE) %>%
    split_dplyr_groups() %>%
    lapply(
      function(x) {
        estimate <- cal_linear_impl_single(
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

cal_linear_impl_single <- function(.data, truth, estimate, run_model, ...) {
  truth <- ensym(truth)

  if (run_model == "linear_spline") {
    f_model <- expr(!!truth ~ s(!!estimate))
    init_model <- mgcv::gam(f_model, data = .data, ...)
    model <- butcher::butcher(init_model)
  }

  if (run_model == "glm") {
    f_model <- expr(!!truth ~ !!estimate)
    init_model <- glm(f_model, data = .data, ...)
    model <- butcher::butcher(init_model)
  }

  model
}


#' @export
print.cal_estimate_linear <- function(x, ...) {
  print_reg_cal(x, upv = FALSE, ...)
}
