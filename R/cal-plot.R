#--------------------------------- Plots ---------------------------------------
#------------------------------- >> Breaks -------------------------------------
#' Probability Calibration plots
#'
#' @description Calibration plot functions. They require a data.frame that contains
#' the predictions and probability columns. The output is a `ggplot2` graph.
#'
#' @details
#' - `cal_plot_breaks()` - Splits the data into bins, based on the
#' number of breaks provided (`num_breaks`). The bins are even ranges, starting
#' at 0, and ending at 1.
#' - `cal_plot_logistic()` - Fits a logistic spline regression (GAM)
#' against the data. It then creates a table with the predictions based on 100
#' probabilities starting at 0, and ending at1.
#' - `cal_plot_windowed()` - Creates a running percentage of the probability
#' that moves across the proportion of events.
#'
#' @param .data A data.frame object containing predictions and probability columns.
#' @param truth The column identifier for the true class results
#' (that is a factor). This should be an unquoted column name.
#' @param estimate The column identifier for the prediction probabilities.
#' This should be an unquoted column name
#' @param group The column identifier to group the results.
#' @param event_level  single string. Either "first" or "second" to specify which
#' level of truth to consider as the "event".
#' @param num_breaks The number of segments to group the probabilities. It
#' defaults to 10.
#' @param window_size The size of segments. Used for the windowed probability
#' calculations. It defaults to 10% of segments.
#' @param step_size The gap between segments. Used for the windowed probability
#' calculations. It defaults to half the size of `window_size`
#' @param conf_level Confidence level to use in the visualization. It defaults
#' to 0.9.
#' @param smooth Applies to the logistic models. It switches between logistic
#' spline when `TRUE`, and simple logistic regression when `FALSE`.
#' @param include_ribbon Flag that indicates if the ribbon layer is to be
#' included. It defaults to `TRUE`.
#' @param include_rug Flag that indicates if the Rug layer is to be included.
#' It defaults to `TRUE`. In the plot, the top side shows the frequency the
#' event occurring, and the bottom the frequency of the event not occurring.
#' @param include_points Flag that indicates if the point layer is to be included.
#' @param ... Additional arguments passed to the `tune_results` object.
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
#'
#' @export
cal_plot_breaks <- function(.data,
                            truth = NULL,
                            estimate = NULL,
                            group = NULL,
                            num_breaks = 10,
                            conf_level = 0.90,
                            include_ribbon = TRUE,
                            include_rug = TRUE,
                            include_points = TRUE,
                            event_level = c("first", "second"),
                            ...) {
  UseMethod("cal_plot_breaks")
}

cal_plot_breaks_impl <- function(.data,
                                 truth = NULL,
                                 estimate = NULL,
                                 group = NULL,
                                 num_breaks = 10,
                                 conf_level = 0.90,
                                 include_ribbon = TRUE,
                                 include_rug = TRUE,
                                 include_points = TRUE,
                                 event_level = c("first", "second"),
                                 ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  assert_truth_two_levels(.data, !!truth)

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
    include_points = include_points
  )
}

#' @export
#' @rdname cal_plot_breaks
cal_plot_breaks.data.frame <- cal_plot_breaks_impl

#' @export
#' @rdname cal_plot_breaks
cal_plot_breaks.tune_results <- function(.data,
                                         truth = NULL,
                                         estimate = NULL,
                                         group = NULL,
                                         num_breaks = 10,
                                         conf_level = 0.90,
                                         include_ribbon = TRUE,
                                         include_rug = TRUE,
                                         include_points = TRUE,
                                         event_level = c("first", "second"),
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
    event_level = event_level
  )
}

#------------------------------ >> Logistic ------------------------------------

#' @rdname cal_plot_breaks
#' @export
cal_plot_logistic <- function(.data,
                              truth = NULL,
                              estimate = NULL,
                              group = NULL,
                              conf_level = 0.90,
                              smooth = TRUE,
                              include_rug = TRUE,
                              include_ribbon = TRUE,
                              event_level = c("first", "second"),
                              ...) {
  UseMethod("cal_plot_logistic")
}


cal_plot_logistic_impl <- function(.data,
                                   truth = NULL,
                                   estimate = NULL,
                                   group = NULL,
                                   conf_level = 0.90,
                                   smooth = TRUE,
                                   include_rug = TRUE,
                                   include_ribbon = TRUE,
                                   event_level = c("first", "second"),
                                   ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  assert_truth_two_levels(.data, !!truth)

  prob_tbl <- .cal_binary_table_logistic(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    conf_level = conf_level,
    event_level = event_level,
    smooth = smooth
  )

  binary_plot_impl(
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
    include_points = FALSE
  )
}

#' @export
#' @rdname cal_plot_breaks
cal_plot_logistic.data.frame <- cal_plot_logistic_impl

#' @export
#' @rdname cal_plot_breaks
cal_plot_logistic.tune_results <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           group = NULL,
                                           conf_level = 0.90,
                                           smooth = TRUE,
                                           include_rug = TRUE,
                                           include_ribbon = TRUE,
                                           event_level = c("first", "second"),
                                           ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ group }},
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
    smooth = smooth
  )
}

#----------------------------- >> Windowed -------------------------------------

#' @rdname cal_plot_breaks
#' @export
cal_plot_windowed <- function(.data,
                              truth = NULL,
                              estimate = NULL,
                              group = NULL,
                              window_size = 0.1,
                              step_size = window_size / 2,
                              conf_level = 0.90,
                              include_ribbon = TRUE,
                              include_rug = TRUE,
                              include_points = TRUE,
                              event_level = c("first", "second"),
                              ...) {
  UseMethod("cal_plot_windowed")
}

cal_plot_windowed_impl <- function(.data,
                                   truth = NULL,
                                   estimate = NULL,
                                   group = NULL,
                                   window_size = 0.1,
                                   step_size = window_size / 2,
                                   conf_level = 0.90,
                                   include_ribbon = TRUE,
                                   include_rug = TRUE,
                                   include_points = TRUE,
                                   event_level = c("first", "second"),
                                   ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  assert_truth_two_levels(.data, !!truth)

  prob_tbl <- .cal_binary_table_windowed(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    window_size = window_size,
    step_size = step_size,
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
    x_label = "Window Midpoint",
    y_label = "Event Rate",
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points
  )
}

#' @export
#' @rdname cal_plot_breaks
cal_plot_windowed.data.frame <- cal_plot_windowed_impl

#' @export
#' @rdname cal_plot_breaks
cal_plot_windowed.tune_results <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           group = NULL,
                                           window_size = 0.1,
                                           step_size = window_size / 2,
                                           conf_level = 0.90,
                                           include_ribbon = TRUE,
                                           include_rug = TRUE,
                                           include_points = TRUE,
                                           event_level = c("first", "second"),
                                           ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ group }},
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
    event_level = event_level
  )
}

#------------------------------- >> Utils --------------------------------------

binary_plot_impl <- function(tbl, x, y,
                             .data, truth, estimate, group,
                             x_label, y_label,
                             include_ribbon, include_rug, include_points) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  x <- enquo(x)
  y <- enquo(y)

  gp_vars <- dplyr::group_vars(.data)

  if (length(gp_vars)) {
    if (length(gp_vars) > 1) {
      rlang::abort("Plot does not support more than one grouping variable")
    }
    has_groups <- TRUE
    dplyr_group <- parse_expr(gp_vars)
    grouping_var <- tbl[, gp_vars][[1]]
    if(is.numeric(grouping_var)) {
      tbl[, gp_vars] <- as.factor(format(grouping_var))
    }
  } else {
    has_groups <- FALSE
    dplyr_group <- NULL
  }

  res <- ggplot(data = tbl, aes(x = !!x, color = !!dplyr_group, fill = !!dplyr_group)) +
    geom_abline(col = "#aaaaaa", linetype = 2) +
    geom_line(aes(y = !!y))

  if (include_points) {
    res <- res + geom_point(aes(y = !!y))
  }

  if (include_ribbon) {
    res <- res +
      geom_ribbon(
        aes(y = !!y, ymin = lower, ymax = upper),
        color = "#ffffff00",
        alpha = 0.08
      )
  }

  if (include_rug & !has_groups) {
    truth_values <- 1:2
    side_values <- c("t", "b")
    for (i in seq_along(truth_values)) {
      level_tbl <- dplyr::filter(.data, as.integer(!!truth) == truth_values[i])
      res <- res +
        geom_rug(
          data = level_tbl,
          aes(x = !!estimate),
          color = "#999999",
          sides = side_values[i],
          length = unit(0.015, "npc"),
          alpha = 0.7,
          show.legend = FALSE
        )
    }
  }

  res <- res +
    lims(x = 0:1, y = 0:1) +
    labs(
      x = x_label,
      y = y_label
    ) +
    theme_light() +
    theme(aspect.ratio = 1)

  if (!quo_is_null(group)) {
    res <- res + facet_wrap(group)
  }

  res
}

#--------------------------------- Tables --------------------------------------
#------------------------------- >> Breaks -------------------------------------

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
                                     event_level = c("first", "second"),
                                     ...) {
  UseMethod(".cal_binary_table_breaks")
}


.cal_binary_table_breaks_impl <- function(.data,
                                          truth,
                                          estimate,
                                          group,
                                          num_breaks = 10,
                                          conf_level = 0.90,
                                          event_level = c("first", "second"),
                                          ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  tbls <- .data %>%
    dplyr::group_by(!!group, .add = TRUE) %>%
    dplyr::group_map(~ {
      grp <- .cal_binary_table_breaks_grp(
        .data = .x,
        truth = !!truth,
        estimate = !!estimate,
        num_breaks = num_breaks,
        conf_level = conf_level,
        event_level = event_level
      )
      dplyr::bind_cols(.y, grp)
    })
  dplyr::bind_rows(tbls)
}

.cal_binary_table_breaks_grp <- function(.data,
                                         truth,
                                         estimate,
                                         group,
                                         num_breaks = 10,
                                         conf_level = 0.90,
                                         event_level = c("first", "second"),
                                         ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  side <- seq(0, 1, by = 1 / num_breaks)

  cuts <- list(
    lower_cut = side[1:length(side) - 1],
    upper_cut = side[2:length(side)]
  )

  .cal_groups(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    cuts = cuts,
    lev = lev,
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
                                                  event_level = c("first", "second"),
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

#------------------------------ >> Logistic ------------------------------------


#' @rdname cal_binary_tables
#' @export
#' @keywords internal
.cal_binary_table_logistic <- function(.data,
                                       truth = NULL,
                                       estimate = NULL,
                                       group = NULL,
                                       conf_level = 0.90,
                                       event_level = c("first", "second"),
                                       ...) {
  UseMethod(".cal_binary_table_logistic")
}

.cal_binary_table_logistic_impl <- function(.data,
                                            truth = NULL,
                                            estimate = NULL,
                                            group = NULL,
                                            conf_level = 0.90,
                                            event_level = c("first", "second"),
                                            smooth = TRUE,
                                            ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  tbls <- .data %>%
    dplyr::group_by(!!group, .add = TRUE) %>%
    dplyr::group_map(~ {
      grp <- .cal_binary_table_logistic_grp(
        .data = .x,
        truth = !!truth,
        estimate = !!estimate,
        conf_level = conf_level,
        event_level = event_level,
        smooth = smooth
      )
      dplyr::bind_cols(.y, grp)
    })
  dplyr::bind_rows(tbls)
}

.cal_binary_table_logistic_grp <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           conf_level = 0.90,
                                           event_level = c("first", "second"),
                                           smooth = TRUE,
                                           ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  prep_data <- dplyr::select(.data, truth = !!truth, estimate = !!estimate)

  if (smooth) {
    model <- mgcv::gam(
      truth ~ s(estimate, k = 10),
      data = prep_data,
      family = binomial()
    )
  } else {
    model <- stats::glm(
      truth ~ estimate,
      data = prep_data,
      family = binomial()
    )
  }

  new_seq <- seq(0, 1, by = .01)
  new_data <- data.frame(estimate = new_seq)
  preds <- predict(model, new_data, se.fit = TRUE)

  if (lev == 1) {
    preds$fit <- -preds$fit
  }

  res <- tibble::tibble(
    prob = binomial()$linkinv(preds$fit),
    lower = binomial()$linkinv(preds$fit - qnorm(conf_level) * preds$se.fit),
    upper = binomial()$linkinv(preds$fit + qnorm(conf_level) * preds$se.fit)
  )

  res <- cbind(new_data, res)

  tibble::as_tibble(res)
}

#' @export
#' @keywords internal
.cal_binary_table_logistic.data.frame <- .cal_binary_table_logistic_impl

#' @export
#' @keywords internal
.cal_binary_table_logistic.tune_results <- function(.data,
                                                    truth = NULL,
                                                    estimate = NULL,
                                                    group = NULL,
                                                    conf_level = 0.90,
                                                    event_level = c("first", "second"),
                                                    ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ group }},
    event_level = event_level,
    ...
  )

  .cal_binary_table_logistic_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    conf_level = conf_level,
    event_level = event_level
  )
}

#----------------------------- >> Windowed -------------------------------------

#' @rdname cal_binary_tables
#' @export
#' @keywords internal
.cal_binary_table_windowed <- function(.data,
                                       truth = NULL,
                                       estimate = NULL,
                                       group = NULL,
                                       window_size = 0.1,
                                       step_size = window_size / 2,
                                       conf_level = 0.90,
                                       event_level = c("first", "second"),
                                       ...) {
  UseMethod(".cal_binary_table_windowed")
}

.cal_binary_table_windowed_impl <- function(.data,
                                            truth = NULL,
                                            estimate = NULL,
                                            group = NULL,
                                            window_size = 0.1,
                                            step_size = window_size / 2,
                                            conf_level = 0.90,
                                            event_level = c("first", "second"),
                                            ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  tbls <- .data %>%
    dplyr::group_by(!!group, .add = TRUE) %>%
    dplyr::group_map(~ {
      grp <- .cal_binary_table_windowed_grp(
        .data = .x,
        truth = !!truth,
        estimate = !!estimate,
        window_size = window_size,
        step_size = step_size,
        conf_level = conf_level,
        event_level = event_level
      )
      dplyr::bind_cols(.y, grp)
    })
  dplyr::bind_rows(tbls)
}

.cal_binary_table_windowed_grp <- function(.data,
                                           truth,
                                           estimate,
                                           window_size = 0.1,
                                           step_size = window_size / 2,
                                           conf_level = 0.90,
                                           event_level = c("first", "second"),
                                           ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  steps <- seq(0, 1, by = step_size)
  cuts <- list()
  cuts$lower_cut <- steps - (window_size / 2)
  cuts$lower_cut[cuts$lower_cut < 0] <- 0
  cuts$upper_cut <- steps + (window_size / 2)
  cuts$upper_cut[cuts$upper_cut > 1] <- 1

  .cal_groups(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    cuts = cuts,
    lev = lev,
    conf_level = conf_level
  )
}

#' @export
#' @keywords internal
.cal_binary_table_windowed.data.frame <- .cal_binary_table_windowed_impl

#' @export
#' @keywords internal
.cal_binary_table_windowed.tune_results <- function(.data,
                                                    truth = NULL,
                                                    estimate = NULL,
                                                    group = NULL,
                                                    window_size = 0.1,
                                                    step_size = window_size / 2,
                                                    conf_level = 0.90,
                                                    event_level = c("first", "second"),
                                                    ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = {{ group }},
    event_level = event_level,
    ...
  )

  .cal_binary_table_windowed_impl(
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

#------------------------------- >> Utils --------------------------------------

process_midpoint <- function(.data,
                             truth,
                             estimate,
                             group = NULL,
                             .bin = NULL,
                             level = 1,
                             conf_level = 0.95) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)
  .bin <- enquo(.bin)

  tbl <- .data %>%
    dplyr::mutate(
      .bin = !!.bin,
      .is_val = ifelse(as.integer(!!truth) == level, 1, 0)
    )

  if (!quo_is_null(group)) tbl <- dplyr::group_by(tbl, !!group, .add = TRUE)
  if (!quo_is_null(.bin)) tbl <- dplyr::group_by(tbl, !!.bin, .add = TRUE)

  tbl <- tbl %>%
    dplyr::summarise(
      event_rate = sum(.is_val, na.rm = TRUE) / dplyr::n(),
      events = sum(.is_val, na.rm = TRUE),
      total = dplyr::n()
    ) %>%
    # dplyr::ungroup() %>%
    dplyr::filter(total > 0)

  if (!quo_is_null(.bin)) tbl <- dplyr::select(tbl, -.bin)

  add_conf_intervals(
    .data = tbl,
    events = events,
    total = total,
    conf_level = conf_level
  )
}

add_conf_intervals <- function(.data,
                               events = events,
                               total = total,
                               conf_level = 0.90) {
  events <- enquo(events)
  total <- enquo(total)
  .data %>%
    purrr::transpose() %>%
    purrr::map_df(
      ~ {
        events <- .x[[as_name(events)]]
        total <- .x[[as_name(total)]]
        suppressWarnings(
          pt <- prop.test(events, total, conf.level = conf_level)
        )
        ret <- tibble::as_tibble(.x)
        ret$lower <- pt$conf.int[[1]]
        ret$upper <- pt$conf.int[[2]]
        ret
      }
    )
}

process_level <- function(x) {
  x <- x[[1]]
  ret <- NULL
  if (x == "first") {
    ret <- 1
  }
  if (x == "second") {
    ret <- 2
  }
  if (is.null(ret)) {
    rlang::abort("Invalid event_level entry. Valid entries are 'first' and 'second'")
  }
  ret
}

assert_truth_two_levels <- function(.data, truth) {
  truth <- enquo(truth)
  if (!quo_is_null(truth)) {
    truth_name <- as_name(truth)
    truth_levels <- levels(.data[truth_name][[1]])
    if (length(truth_levels) != 2) {
      rlang::abort(paste0("'", truth_name, "' does not have 2 levels"))
    }
  }
}

tune_results_args <- function(.data,
                              truth,
                              estimate,
                              group,
                              event_level,
                              parameters = NULL,
                              ...
                              ) {
  if (!(".predictions" %in% colnames(.data))) {
    rlang::abort(
      paste0(
        "The `tune_results` object does not contain the `.predictions` column.",
        " Refit with the control argument `save_pred = TRUE` to save predictions."
      )
    )
  }

  predictions <- tune::collect_predictions(x = .data,
                                           summarize = TRUE,
                                           parameters = parameters,
                                           ...
                                           )

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  if (quo_is_null(truth)) {
    truth_str <- attributes(.data)$outcome
    truth <- parse_expr(truth_str)
  }

  if (quo_is_null(estimate)) {
    truth_str <- as_name(truth)
    lev <- process_level(event_level)
    fc_truth <- levels(predictions[[truth_str]])
    estimate_str <- paste0(".pred_", fc_truth[[lev]])
    estimate <- parse_expr(estimate_str)
  }

  if (quo_is_null(group)) {
    group <- quo(.config)
  }

  list(
    truth = quo(!!truth),
    estimate = quo(!!estimate),
    group = quo(!!group),
    predictions = predictions
  )
}

.cal_groups <- function(.data, truth, estimate, cuts, lev, conf_level) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)


  cuts %>%
    purrr::transpose() %>%
    purrr::map_df(
      ~ {
        .data %>%
          dplyr::filter(
            !!estimate >= !!.x$lower_cut & !!estimate <= !!.x$upper_cut
          ) %>%
          process_midpoint(
            truth = !!truth,
            estimate = !!estimate,
            level = lev,
            conf_level = conf_level
          ) %>%
          dplyr::mutate(
            predicted_midpoint = .x$lower_cut + ((.x$upper_cut - .x$lower_cut) / 2)
          ) %>%
          dplyr::select(predicted_midpoint, dplyr::everything())
      }
    )
}
