#---------------------------- Plot methods -------------------------------------
#' Probability calibration plots via binning
#'
#' @description
#' A plot is created to assess whether the observed rate of the event is about
#' the same as the predicted probability of the event from some model.
#'
#' A sequence of even, mutually exclusive bins are created from zero to one.
#' For each bin, the data whose predicted probability falls within the range
#' of the bin is used to calculate the observed event rate (along with confidence
#' intervals for the event rate).

#' If the predictions are well calibrated, the fitted curve should align with
#' the diagonal line.
#'
#' @param .data A data.frame object containing predictions and probability columns.
#' @param truth The column identifier for the true class results
#' (that is a factor). This should be an unquoted column name.
#' @param estimate A vector of column identifiers, or one of `dplyr` selector
#' functions to choose which variables contains the class probabilities. It
#' defaults to the prefix used by tidymodels (`.pred_`). The order of the
#' identifiers will be considered the same as the order of the levels of the
#' `truth` variable.
#' @param group The column identifier to group the results.
#' @param event_level  single string. Either "first" or "second" to specify which
#' level of truth to consider as the "event". Defaults to "auto", which allows
#' the function decide which one to use based on the type of model (binary,
#' multi-class or linear)
#' @param num_breaks The number of segments to group the probabilities. It
#' defaults to 10.
#' @param conf_level Confidence level to use in the visualization. It defaults
#' to 0.9.
#' @param include_ribbon Flag that indicates if the ribbon layer is to be
#' included. It defaults to `TRUE`.
#' @param include_rug Flag that indicates if the Rug layer is to be included.
#' It defaults to `TRUE`. In the plot, the top side shows the frequency the
#' event occurring, and the bottom the frequency of the event not occurring.
#' @param include_points Flag that indicates if the point layer is to be included.
#' @param ... Additional arguments passed to the `tune_results` object.
#' @return A ggplot object.
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' cal_plot_breaks(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' cal_plot_logistic(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' cal_plot_windowed(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' # The functions support dplyr groups
#'
#' model <- glm(Class ~ .pred_good, segment_logistic, family = "binomial")
#'
#' preds <- predict(model, segment_logistic, type = "response")
#'
#' gl <- segment_logistic %>%
#'   mutate(.pred_good = 1 - preds, source = "glm")
#'
#' combined <- bind_rows(mutate(segment_logistic, source = "original"), gl)
#'
#' combined %>%
#'   group_by(source) %>%
#'   cal_plot_logistic(Class, .pred_good)
#'
#' # The grouping can be faceted in ggplot2
#' combined %>%
#'   group_by(source) %>%
#'   cal_plot_logistic(Class, .pred_good) +
#'   facet_wrap(~source) +
#'   theme(legend.position = "")
#' @seealso [cal_plot_logistic()], [cal_plot_windowed()]
#' @export
cal_plot_breaks <- function(.data,
                            truth = NULL,
                            estimate = dplyr::starts_with(".pred"),
                            group = NULL,
                            num_breaks = 10,
                            conf_level = 0.90,
                            include_ribbon = TRUE,
                            include_rug = TRUE,
                            include_points = TRUE,
                            event_level = c("auto", "first", "second"),
                            ...) {
  UseMethod("cal_plot_breaks")
}

cal_plot_breaks_impl <- function(.data,
                                 truth = NULL,
                                 estimate = dplyr::starts_with(".pred"),
                                 group = NULL,
                                 num_breaks = 10,
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

  prob_tbl <- .cal_binary_table_breaks(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    num_breaks = num_breaks,
    conf_level = conf_level,
    event_level = event_level
  )

  binary_plot_impl(
    tbl = prob_tbl,
    x = predicted_midpoint,
    y = event_rate,
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    x_label = "Bin Midpoint",
    y_label = "Event Rate",
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points,
    is_tune_results = is_tune_results
  )
}

#' @export
#' @rdname cal_plot_breaks
cal_plot_breaks.data.frame <- cal_plot_breaks_impl

#' @export
#' @rdname cal_plot_breaks
cal_plot_breaks.tune_results <- function(.data,
                                         truth = NULL,
                                         estimate = dplyr::starts_with(".pred"),
                                         group = NULL,
                                         num_breaks = 10,
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
    group = {{ group }},
    event_level = event_level,
    ...
  )

  cal_plot_breaks_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    num_breaks = num_breaks,
    conf_level = conf_level,
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points,
    event_level = event_level,
    is_tune_results = TRUE
  )
}


#----------------------------- Table methods -----------------------------------

#' Probability Calibration table
#'
#' @description Calibration table functions. They require a data.frame that
#' contains the predictions and probability columns. The output is another
#' `tibble` with segmented data that compares the accuracy of the probability
#' to the actual outcome.
#'
#' @details
#' - `.cal_binary_table_breaks()` - Splits the data into bins, based on the
#' number of breaks provided (`num_breaks`). The bins are even ranges, starting
#' at 0, and ending at 1.
#' - `.cal_binary_table_logistic()` - Fits a logistic spline regression (GAM)
#' against the data. It then creates a table with the predictions based on 100
#' probabilities starting at 0, and ending at 1.
#' - `.cal_binary_table_windowed()` - Creates a running percentage of the
#' probability that moves across the proportion of events.
#'
#' @inheritParams cal_plot_breaks
#'
#' @examples
#' .cal_binary_table_breaks(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' .cal_binary_table_logistic(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' .cal_binary_table_windowed(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#' @rdname cal_binary_tables
#' @export
#' @keywords internal
.cal_binary_table_breaks <- function(.data,
                                     truth = NULL,
                                     estimate = NULL,
                                     group = NULL,
                                     num_breaks = 10,
                                     conf_level = 0.90,
                                     event_level = c("auto", "first", "second"),
                                     ...) {
  UseMethod(".cal_binary_table_breaks")
}


.cal_binary_table_breaks_impl <- function(.data,
                                          truth,
                                          estimate,
                                          group,
                                          num_breaks = 10,
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
      grp <- .cal_binary_table_breaks_grp(
        .data = .x,
        truth = !!truth,
        num_breaks = num_breaks,
        conf_level = conf_level,
        event_level = event_level,
        levels = levels
      )
      dplyr::bind_cols(.y, grp)
    }) %>%
    dplyr::bind_rows()

  if (length(levels) > 2) {
    res <- dplyr::group_by(res, !!truth)
  }

  res
}

.cal_binary_table_breaks_grp <- function(.data,
                                         truth,
                                         group,
                                         num_breaks = 10,
                                         conf_level = 0.90,
                                         event_level = c("auto", "first", "second"),
                                         levels,
                                         ...) {

  side <- seq(0, 1, by = 1 / num_breaks)

  cuts <- list(
    lower_cut = side[1:length(side) - 1],
    upper_cut = side[2:length(side)]
  )

  .cal_class_grps(
    .data = .data,
    truth = {{ truth }},
    cuts = cuts,
    levels = levels,
    event_level = event_level,
    conf_level = conf_level
  )
}

#' @export
#' @keywords internal
.cal_binary_table_breaks.data.frame <- .cal_binary_table_breaks_impl

#' @export
#' @keywords internal
.cal_binary_table_breaks.tune_results <- function(.data,
                                                  truth = NULL,
                                                  estimate = NULL,
                                                  group = NULL,
                                                  num_breaks = 10,
                                                  conf_level = 0.90,
                                                  event_level = c("auto", "first", "second"),
                                                  ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ group }},
    event_level = event_level,
    ...
  )

  .cal_binary_table_breaks_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    num_breaks = num_breaks,
    conf_level = conf_level,
    event_level = event_level
  )
}
