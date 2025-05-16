#-------------------------------- Plot -----------------------------------------
#------------------------------ >> Methods -------------------------------------
#' Probability calibration plots via logistic regression
#'
#' @inheritParams cal_plot_breaks
#'
#' @description
#' A logistic regression model is fit where the original outcome data are used
#' as the outcome and the estimated class probabilities for one class are used
#' as the predictor. If `smooth = TRUE`, a generalized additive model is fit
#' using [mgcv::gam()] and the default smoothing method. Otherwise, a simple
#' logistic regression is used.
#'
#' If the predictions are well calibrated, the fitted curve should align with
#' the diagonal line. Confidence intervals for the fitted line are also
#' shown.
#' @param smooth A logical for using a generalized additive model with smooth
#' terms for the predictor via [mgcv::gam()] and [mgcv::s()].
#' @return A ggplot object.
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_plot_windowed()], [cal_plot_breaks()]
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' cal_plot_logistic(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' cal_plot_logistic(
#'   segment_logistic,
#'   Class,
#'   .pred_good,
#'   smooth = FALSE
#' )
#' @seealso [cal_plot_breaks()], [cal_plot_windowed()]
#' @export
cal_plot_logistic <- function(.data,
                              truth = NULL,
                              estimate = dplyr::starts_with(".pred"),
                              conf_level = 0.90,
                              smooth = TRUE,
                              include_rug = TRUE,
                              include_ribbon = TRUE,
                              event_level = c("auto", "first", "second"),
                              ...) {
  UseMethod("cal_plot_logistic")
}

#' @export
#' @rdname cal_plot_logistic
cal_plot_logistic.data.frame <- function(.data,
                                         truth = NULL,
                                         estimate = dplyr::starts_with(".pred"),
                                         conf_level = 0.90,
                                         smooth = TRUE,
                                         include_rug = TRUE,
                                         include_ribbon = TRUE,
                                         event_level = c("auto", "first", "second"),
                                         ...,
                                         .by = NULL) {
  group <- get_group_argument({{ .by }}, .data)
  .data <- dplyr::group_by(.data, dplyr::across({{ group }}))

  cal_plot_logistic_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ group }},
    conf_level = conf_level,
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    event_level = event_level,
    smooth = smooth,
    is_tune_results = FALSE
  )
}
#' @export
#' @rdname cal_plot_logistic
cal_plot_logistic.tune_results <- function(.data,
                                           truth = NULL,
                                           estimate = dplyr::starts_with(".pred"),
                                           conf_level = 0.90,
                                           smooth = TRUE,
                                           include_rug = TRUE,
                                           include_ribbon = TRUE,
                                           event_level = c("auto", "first", "second"),
                                           ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    event_level = event_level,
    ...
  )

  cal_plot_logistic_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    conf_level = conf_level,
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    event_level = event_level,
    smooth = smooth,
    is_tune_results = TRUE
  )
}

#' @export
#' @rdname cal_plot_logistic
cal_plot_logistic.grouped_df <- function(.data,
                                         truth = NULL,
                                         estimate = NULL,
                                         conf_level = 0.90,
                                         smooth = TRUE,
                                         include_rug = TRUE,
                                         include_ribbon = TRUE,
                                         event_level = c("auto", "first", "second"),
                                         ...) {
  abort_if_grouped_df()
}

#--------------------------- >> Implementation ---------------------------------
cal_plot_logistic_impl <- function(.data,
                                   truth = NULL,
                                   estimate = dplyr::starts_with(".pred"),
                                   group = NULL,
                                   conf_level = 0.90,
                                   smooth = TRUE,
                                   include_rug = TRUE,
                                   include_ribbon = TRUE,
                                   event_level = c("auto", "first", "second"),
                                   is_tune_results = FALSE,
                                   ...) {
  rlang::arg_match0(event_level, c("auto", "first", "second"), error_call = NULL)
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  prob_tbl <- .cal_table_logistic(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    conf_level = conf_level,
    event_level = event_level,
    smooth = smooth
  )

  cal_plot_impl(
    tbl = prob_tbl,
    x = estimate,
    y = prob,
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    x_label = "Probability",
    y_label = "Predicted Event Rate",
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = FALSE,
    is_tune_results = is_tune_results
  )
}

#---------------------------------- Table --------------------------------------
#------------------------------- >> Methods ------------------------------------
#' @rdname cal_binary_tables
#' @export
#' @keywords internal
.cal_table_logistic <- function(.data,
                                truth = NULL,
                                estimate = NULL,
                                .by = NULL,
                                conf_level = 0.90,
                                smooth = TRUE,
                                event_level = c("auto", "first", "second"),
                                ...) {
  UseMethod(".cal_table_logistic")
}

#' @export
#' @keywords internal
.cal_table_logistic.data.frame <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           .by = NULL,
                                           conf_level = 0.90,
                                           smooth = TRUE,
                                           event_level = c("auto", "first", "second"),
                                           ...) {
  .cal_table_logistic_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ .by }},
    conf_level = conf_level,
    smooth = smooth,
    event_level = event_level
  )
}

#' @export
#' @keywords internal
.cal_table_logistic.tune_results <- function(.data,
                                             truth = NULL,
                                             estimate = NULL,
                                             .by = NULL,
                                             conf_level = 0.90,
                                             smooth = TRUE,
                                             event_level = c("auto", "first", "second"),
                                             ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    event_level = event_level,
    ...
  )

  .cal_table_logistic_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    smooth = smooth,
    conf_level = conf_level,
    event_level = event_level
  )
}

#--------------------------- >> Implementation ---------------------------------
.cal_table_logistic_impl <- function(.data,
                                     truth = NULL,
                                     estimate = NULL,
                                     group = NULL,
                                     conf_level = 0.90,
                                     event_level = c("auto", "first", "second"),
                                     smooth = TRUE,
                                     ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  levels <- truth_estimate_map(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate
  )

  res <- .data |>
    dplyr::group_by(!!group, .add = TRUE) |>
    dplyr::group_nest(.key = "cal_data") |>
    dplyr::mutate(
      res = map(
        cal_data,
        .cal_class_grps,
        truth = !!truth,
        levels = levels,
        conf_level = conf_level,
        event_level = event_level,
        smooth = smooth,
        method = "model"
      )
    ) |>
    dplyr::select(-cal_data) |>
    tidyr::unnest(cols = c(res))

  if (length(levels) > 2) {
    res <- dplyr::group_by(res, !!truth, .add = TRUE)
  }

  res
}
