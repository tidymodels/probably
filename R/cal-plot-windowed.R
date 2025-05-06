#-------------------------------- Plot -----------------------------------------
#------------------------------ >> Methods -------------------------------------
#' Probability calibration plots via moving windows
#'
#' @description
#' A plot is created to assess whether the observed rate of the event is about
#' the sample as the predicted probability of the event from some model. This
#' is similar to [cal_plot_breaks()], except that the bins are overlapping.
#'
#' A sequence of bins are created from zero to one. For each bin, the data whose
#' predicted probability falls within the range of the bin is used to calculate
#' the observed event rate (along with confidence intervals for the event rate).
#'
#' If the predictions are well calibrated, the fitted curve should align with
#' the diagonal line.
#' @param window_size The size of segments. Used for the windowed probability
#' calculations. It defaults to 10% of segments.
#' @param step_size The gap between segments. Used for the windowed probability
#' calculations. It defaults to half the size of `window_size`
#' @return A ggplot object.
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_plot_logistic()], [cal_plot_breaks()]
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' cal_plot_windowed(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' # More breaks
#' cal_plot_windowed(
#'   segment_logistic,
#'   Class,
#'   .pred_good,
#'   window_size = 0.05
#' )
#' @inheritParams cal_plot_breaks
#' @seealso [cal_plot_breaks()], [cal_plot_logistic()]
#' @export
cal_plot_windowed <- function(.data,
                              truth = NULL,
                              estimate = dplyr::starts_with(".pred"),
                              window_size = 0.1,
                              step_size = window_size / 2,
                              conf_level = 0.90,
                              include_ribbon = TRUE,
                              include_rug = TRUE,
                              include_points = TRUE,
                              event_level = c("auto", "first", "second"),
                              ...) {
  UseMethod("cal_plot_windowed")
}

#' @export
#' @rdname cal_plot_windowed
cal_plot_windowed.data.frame <- function(.data,
                                         truth = NULL,
                                         estimate = dplyr::starts_with(".pred"),
                                         window_size = 0.1,
                                         step_size = window_size / 2,
                                         conf_level = 0.90,
                                         include_ribbon = TRUE,
                                         include_rug = TRUE,
                                         include_points = TRUE,
                                         event_level = c("auto", "first", "second"),
                                         ...,
                                         .by = NULL) {
  group <- get_group_argument({{ .by }}, .data)
  .data <- dplyr::group_by(.data, dplyr::across({{ group }}))

  cal_plot_windowed_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ group }},
    window_size = window_size,
    step_size = step_size,
    conf_level = conf_level,
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points,
    event_level = event_level,
    is_tune_results = FALSE
  )
}

#' @export
#' @rdname cal_plot_windowed
cal_plot_windowed.tune_results <- function(.data,
                                           truth = NULL,
                                           estimate = dplyr::starts_with(".pred"),
                                           window_size = 0.1,
                                           step_size = window_size / 2,
                                           conf_level = 0.90,
                                           include_ribbon = TRUE,
                                           include_rug = TRUE,
                                           include_points = TRUE,
                                           event_level = c("auto", "first", "second"),
                                           ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    event_level = event_level,
    ...
  )

  cal_plot_windowed_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    window_size = window_size,
    step_size = step_size,
    conf_level = conf_level,
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points,
    event_level = event_level,
    is_tune_results = TRUE
  )
}

#' @export
#' @rdname cal_plot_windowed
cal_plot_windowed.grouped_df <- function(.data,
                                         truth = NULL,
                                         estimate = NULL,
                                         window_size = 0.1,
                                         step_size = window_size / 2,
                                         conf_level = 0.90,
                                         include_ribbon = TRUE,
                                         include_rug = TRUE,
                                         include_points = TRUE,
                                         event_level = c("auto", "first", "second"),
                                         ...) {
  abort_if_grouped_df()
}

#--------------------------- >> Implementation ---------------------------------
cal_plot_windowed_impl <- function(.data,
                                   truth = NULL,
                                   estimate = dplyr::starts_with(".pred"),
                                   group = NULL,
                                   window_size = 0.1,
                                   step_size = window_size / 2,
                                   conf_level = 0.90,
                                   include_ribbon = TRUE,
                                   include_rug = TRUE,
                                   include_points = TRUE,
                                   event_level = c("auto", "first", "second"),
                                   is_tune_results = FALSE,
                                   ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  prob_tbl <- .cal_table_windowed(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    window_size = window_size,
    step_size = step_size,
    conf_level = conf_level,
    event_level = event_level
  )

  cal_plot_impl(
    tbl = prob_tbl,
    x = predicted_midpoint,
    y = event_rate,
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    x_label = "Window Midpoint",
    y_label = "Event Rate",
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points,
    is_tune_results = is_tune_results
  )
}

#---------------------------------- Table --------------------------------------
#------------------------------- >> Methods ------------------------------------
#' @rdname cal_binary_tables
#' @export
#' @keywords internal
.cal_table_windowed <- function(.data,
                                truth = NULL,
                                estimate = NULL,
                                .by = NULL,
                                window_size = 0.1,
                                step_size = window_size / 2,
                                conf_level = 0.90,
                                event_level = c("auto", "first", "second"),
                                ...) {
  UseMethod(".cal_table_windowed")
}

#' @export
#' @keywords internal
.cal_table_windowed.data.frame <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           .by = NULL,
                                           window_size = 0.1,
                                           step_size = window_size / 2,
                                           conf_level = 0.90,
                                           event_level = c("auto", "first", "second"),
                                           ...) {
  .cal_table_windowed_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ .by }},
    window_size = window_size,
    step_size = step_size,
    conf_level = conf_level,
    event_level = event_level
  )
}

#' @export
#' @keywords internal
.cal_table_windowed.tune_results <- function(.data,
                                             truth = NULL,
                                             estimate = NULL,
                                             .by = NULL,
                                             window_size = 0.1,
                                             step_size = window_size / 2,
                                             conf_level = 0.90,
                                             event_level = c("auto", "first", "second"),
                                             ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    event_level = event_level,
    ...
  )

  .cal_table_windowed_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    window_size = window_size,
    step_size = step_size,
    conf_level = conf_level,
    event_level = event_level
  )
}

#--------------------------- >> Implementation ---------------------------------
.cal_table_windowed_impl <- function(.data,
                                     truth = NULL,
                                     estimate = NULL,
                                     group = NULL,
                                     window_size = 0.1,
                                     step_size = window_size / 2,
                                     conf_level = 0.90,
                                     event_level = c("auto", "first", "second"),
                                     ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  levels <- truth_estimate_map(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate
  )

  res <- .data %>%
    dplyr::group_by(!!group, .add = TRUE) %>%
    dplyr::group_map(~ {
      grp <- .cal_table_windowed_grp(
        .data = .x,
        truth = !!truth,
        window_size = window_size,
        step_size = step_size,
        conf_level = conf_level,
        event_level = event_level,
        levels = levels
      )
      dplyr::bind_cols(.y, grp)
    }) %>%
    dplyr::bind_rows()


  if (length(levels) > 2) {
    res <- dplyr::group_by(res, !!truth, .add = TRUE)
  }

  res
}

.cal_table_windowed_grp <- function(.data,
                                    truth,
                                    window_size = 0.1,
                                    step_size = window_size / 2,
                                    conf_level = 0.90,
                                    event_level = c("auto", "first", "second"),
                                    levels = levels,
                                    ...) {
  steps <- seq(0, 1, by = step_size)
  cuts <- list()
  cuts$lower_cut <- steps - (window_size / 2)
  cuts$lower_cut[cuts$lower_cut < 0] <- 0
  cuts$upper_cut <- steps + (window_size / 2)
  cuts$upper_cut[cuts$upper_cut > 1] <- 1

  .cal_class_grps(
    .data = .data,
    truth = {{ truth }},
    cuts = cuts,
    levels = levels,
    event_level = event_level,
    conf_level = conf_level
  )
}
