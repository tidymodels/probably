#--------------------------------- Plots ---------------------------------------
#------------------------------- >> Breaks -------------------------------------
#' Probability Calibration plots
#'
#' @description Calibration plot functions. They require a data.frame that contains
#' the predictions and probability columns. The output is a `ggplot2` graph.
#'
#' @details
#' - `cal_binary_plot_breaks()` - Splits the data into bins, based on the
#' number of breaks provided (`num_breaks`). The bins are even ranges, starting
#' at 0, and ending at 1.
#' - `cal_binary_plot_logistic()` - Fits a logistic spline regression (GAM)
#' against the data. It then creates a table with the predictions based on 100
#' probabilities starting at 0, and ending at1.
#' - `cal_binary_plot_windowed()` - Creates a running percentage of the probability
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
#' spline when `TRUE`, and regular logistic when `FALSE`.
#' @param include_ribbon Flag that indicates if the ribbon layer is to be
#' included. It defaults to `TRUE`.
#' @param include_rug Flag that indicates if the Rug layer is to be included.
#' It defaults to `TRUE`. In the plot, the top side shows the frequency the
#' event occurring, and the bottom the frequency of the event not occurring.
#' @param include_points Flag that indicates if the point layer is to be included.
#' @param ... Additional arguments passed to the `tune_results` object.
#' @seealso These functions depend on tables built by the following corresponding
#' functions: [cal_binary_table_breaks()], [cal_binary_table_logistic()], and
#' [cal_binary_table_windowed()].
#' @examples
#'
#' library(ggplot2)
#'
#' cal_binary_plot_breaks(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' cal_binary_plot_logistic(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' cal_binary_plot_windowed(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' @export
cal_binary_plot_breaks <- function(.data,
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
  UseMethod("cal_binary_plot_breaks")
}

cal_binary_plot_breaks_impl <- function(.data,
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

  prob_tbl <- cal_binary_table_breaks(
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
    x_label = "Predicted Midpoint",
    y_label = "Event Rate",
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points
  )
}

#' @export
cal_binary_plot_breaks.data.frame <- cal_binary_plot_breaks_impl

#' @export
cal_binary_plot_breaks.tune_results <- function(.data,
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

  cal_binary_plot_breaks_impl(
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

#' @rdname cal_binary_plot_breaks
#'
#' @export
cal_binary_plot_logistic <- function(.data,
                                     truth = NULL,
                                     estimate = NULL,
                                     group = NULL,
                                     conf_level = 0.90,
                                     smooth = TRUE,
                                     include_rug = TRUE,
                                     include_ribbon = TRUE,
                                     event_level = c("first", "second"),
                                     ...) {
  UseMethod("cal_binary_plot_logistic")
}


cal_binary_plot_logistic_impl <- function(.data,
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

  prob_tbl <- cal_binary_table_logistic(
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
    x_label = "Estimate",
    y_label = "Probability",
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = FALSE
  )
}

#' @export
cal_binary_plot_logistic.data.frame <- cal_binary_plot_logistic_impl

#' @export
cal_binary_plot_logistic.tune_results <- function(.data,
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

  cal_binary_plot_logistic_impl(
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

#' @rdname cal_binary_plot_breaks
#' @export
cal_binary_plot_windowed <- function(.data,
                                     truth = NULL,
                                     estimate = NULL,
                                     group = NULL,
                                     window_size = 0.1,
                                     step_size = window_size / 2,
                                     conf_level = 0.90,
                                     include_ribbon = TRUE,
                                     include_rug = TRUE,
                                     include_points = FALSE,
                                     event_level = c("first", "second"),
                                     ...) {
  UseMethod("cal_binary_plot_windowed")
}

cal_binary_plot_windowed_impl <- function(.data,
                                          truth = NULL,
                                          estimate = NULL,
                                          group = NULL,
                                          window_size = 0.1,
                                          step_size = window_size / 2,
                                          conf_level = 0.90,
                                          include_ribbon = TRUE,
                                          include_rug = TRUE,
                                          include_points = FALSE,
                                          event_level = c("first", "second"),
                                          ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  assert_truth_two_levels(.data, !!truth)

  prob_tbl <- cal_binary_table_windowed(
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
    x_label = "Predicted Midpoint",
    y_label = "Event Rate",
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    include_points = include_points
  )
}

#' @export
cal_binary_plot_windowed.data.frame <- cal_binary_plot_windowed_impl

#' @export
cal_binary_plot_windowed.tune_results <- function(.data,
                                                  truth = NULL,
                                                  estimate = NULL,
                                                  group = NULL,
                                                  window_size = 0.1,
                                                  step_size = window_size / 2,
                                                  conf_level = 0.90,
                                                  include_ribbon = TRUE,
                                                  include_rug = TRUE,
                                                  include_points = FALSE,
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

  cal_binary_plot_windowed_impl(
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

  res <- ggplot(data = tbl, aes(x = !!x)) +
    geom_abline(col = "#aaaaaa", linetype = 2) +
    geom_line(aes(y = !!y))

  if (include_points) {
    res <- res + geom_point(aes(y = !!y))
  }

  if (include_ribbon) {
    res <- res +
      geom_ribbon(aes(y = !!y, ymin = lower, ymax = upper), alpha = 0.1)
  }

  if (include_rug) {
    truth_values <- 1:2
    side_values <- c("t", "b")
    for (i in seq_along(truth_values)) {
      level_tbl <- dplyr::filter(.data, as.integer(!!truth) == truth_values[i])
      res <- res +
        geom_rug(
          data = level_tbl,
          aes(x = !!estimate, col = !!truth),
          sides = side_values[i],
          cex = 0.1,
          show.legend = FALSE
        )
    }
  }

  sub_title <- paste0("'", as_name(truth), "' vs '", as_name(estimate), "'")

  res <- res +
    lims(x = 0:1, y = 0:1) +
    labs(
      subtitle = sub_title,
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
#' - `cal_binary_table_breaks()` - Splits the data into bins, based on the
#' number of breaks provided (`num_breaks`). The bins are even ranges, starting
#' at 0, and ending at 1.
#' - `cal_binary_table_logistic()` - Fits a logistic spline regression (GAM)
#' against the data. It then creates a table with the predictions based on 100
#' probabilities starting at 0, and ending at 1.
#' - `cal_binary_table_windowed()` - Creates a running percentage of the
#' probability that moves across the proportion of events.
#' @inheritParams cal_binary_plot_breaks
#'
#' @examples
#' cal_binary_table_breaks(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' cal_binary_table_logistic(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' cal_binary_table_windowed(
#'   segment_logistic,
#'   Class,
#'   .pred_good
#' )
#'
#' @export
cal_binary_table_breaks <- function(.data,
                                    truth = NULL,
                                    estimate = NULL,
                                    group = NULL,
                                    num_breaks = 10,
                                    conf_level = 0.90,
                                    event_level = c("first", "second"),
                                    ...) {
  UseMethod("cal_binary_table_breaks")
}

cal_binary_table_breaks_impl <- function(.data,
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

  lev <- process_level(event_level)

  bin_exprs <- purrr::map(
    seq_len(num_breaks),
    ~ expr(!!estimate <= !!.x / !!num_breaks ~ !!.x)
  )

  .data %>%
    dplyr::mutate(
      .bin = dplyr::case_when(!!!bin_exprs)
    ) %>%
    process_midpoint(
      truth = !!truth,
      estimate = !!estimate,
      group = !!group,
      .bin = .bin,
      level = lev,
      conf_level = conf_level
    )
}

#' @export
cal_binary_table_breaks.data.frame <- cal_binary_table_breaks_impl

#' @export
cal_binary_table_breaks.tune_results <- function(.data,
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

  cal_binary_table_breaks_impl(
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


#' @rdname cal_binary_table_breaks
#' @export
cal_binary_table_logistic <- function(.data,
                                      truth = NULL,
                                      estimate = NULL,
                                      group = NULL,
                                      conf_level = 0.90,
                                      event_level = c("first", "second"),
                                      ...) {
  UseMethod("cal_binary_table_logistic")
}

cal_binary_table_logistic_impl <- function(.data,
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
    dplyr::group_by(!!group) %>%
    dplyr::group_map(~ {
      grp <- cal_binary_table_logistic_grp(
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

cal_binary_table_logistic_grp <- function(.data,
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
cal_binary_table_logistic.data.frame <- cal_binary_table_logistic_impl

#' @export
cal_binary_table_logistic.tune_results <- function(.data,
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

  cal_binary_table_logistic_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    conf_level = conf_level,
    event_level = event_level
  )
}

#----------------------------- >> Windowed -------------------------------------

#' @rdname cal_binary_table_breaks
#' @export
cal_binary_table_windowed <- function(.data,
                                      truth = NULL,
                                      estimate = NULL,
                                      group = NULL,
                                      window_size = 0.1,
                                      step_size = window_size / 2,
                                      conf_level = 0.90,
                                      event_level = c("first", "second"),
                                      ...) {
  UseMethod("cal_binary_table_windowed")
}

cal_binary_table_windowed_impl <- function(.data,
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
    dplyr::group_by(!!group) %>%
    dplyr::group_map(~ {
      grp <- cal_binary_table_windowed_grp(
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

cal_binary_table_windowed_grp <- function(.data,
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

  n_row <- nrow(.data)

  window_number <- round(n_row * window_size)
  step_number <- round(n_row * step_size)

  .data %>%
    dplyr::select(!!truth, !!estimate) %>%
    dplyr::arrange(!!estimate) %>%
    slider::slide(
      ~.x,
      .before = window_number,
      .complete = TRUE,
      .step = step_number
    ) %>%
    purrr::map_df(~ {
      if (!is.null(.x)) {
        process_midpoint(
          .data = .x,
          truth = !!truth,
          estimate = !!estimate,
          level = lev,
          conf_level = conf_level
        )
      }
    })
}

#' @export
cal_binary_table_windowed.data.frame <- cal_binary_table_windowed_impl

#' @export
cal_binary_table_windowed.tune_results <- function(.data,
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

  cal_binary_table_windowed_impl(
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
      predicted_midpoint = median(!!estimate, na.rm = TRUE),
      event_rate = sum(.is_val, na.rm = TRUE) / dplyr::n(),
      events = sum(.is_val, na.rm = TRUE),
      total = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
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

tune_results_args <- function(.data, truth, estimate, group, event_level, ...) {
  if (!(".predictions" %in% colnames(.data))) {
    rlang::abort(
      paste0(
        "The `tune_results` object does not contain the `.predictions` column.",
        " Refit with the control argument `save_pred = TRUE` to save predictions."
      )
    )
  }

  predictions <- tune::collect_predictions(.data, summarize = TRUE, ...)

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
