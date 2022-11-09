#--------------------------------- Plots ---------------------------------------

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
#' @param .data A data.frame object containing predictions and probability columns
#' @param truth The column identifier for the true class results
#' (that is a factor). This should be an unquoted column name
#' @param estimate The column identifier for the prediction probabilities.
#' This should be an unquoted column name
#' @param event_level  single string. Either "first" or "second" to specify which
#' level of truth to consider as the "event".
#' @param num_breaks The number of segments to group the probabilities. Defaults
#' to 10
#' @param window_size The size of segments. Used for the windowed probability
#' calculations
#' @param step_size The gap between segments. Used for the windowed probability
#' calculations
#' @param conf_level Confidence level to use in the visualization. Defaults to 0.9
#' @param include_ribbon Flag that indicates if the ribbon layer is to be
#' included. Defaults to `TRUE`
#' @param include_rug Flag that indicates if the Rug layer is to be included.
#' Defaults to `TRUE`
#' @seealso These functions depend on tables built by the following corresponding
#' functions: [cal_binary_table_breaks()], [cal_binary_table_logistic()], and
#' [cal_binary_table_windowed()]
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
                                   num_breaks = 10,
                                   conf_level = 0.90,
                                   include_ribbon = TRUE,
                                   include_rug = TRUE,
                                   event_level = c("first", "second")) {
  UseMethod("cal_binary_plot_breaks")
}

cal_binary_plot_breaks_impl <- function(.data,
                                        truth = NULL,
                                        estimate = NULL,
                                        num_breaks = 10,
                                        conf_level = 0.90,
                                        include_ribbon = TRUE,
                                        include_rug = TRUE,
                                        event_level = c("first", "second")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  assert_truth_two_levels(.data, !!truth)

  prob_tbl <- cal_binary_table_breaks(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    num_breaks = num_breaks,
    conf_level = conf_level,
    event_level = event_level
  )

  binary_plot_impl(
    prob_tbl, predicted_midpoint, event_rate,
    .data, !!truth, !!estimate,
    "Predicted Midpoint", "Event Rate",
    sub_title, include_ribbon, include_rug, TRUE
  )
}

#' @export
cal_binary_plot_breaks.data.frame <- cal_binary_plot_breaks_impl

#' @export
cal_binary_plot_breaks.tune_results <- function(.data,
                                                truth = NULL,
                                                estimate = NULL,
                                                num_breaks = 10,
                                                conf_level = 0.90,
                                                include_ribbon = TRUE,
                                                include_rug = TRUE,
                                                event_level = c("first", "second")) {
  rs <- tune::collect_predictions(.data, summarize = TRUE)

  te <- tune_results_args(.data, {{ truth }}, {{ estimate }}, event_level, rs)

  cal_binary_plot_breaks_impl(
    .data = rs,
    truth = !!te$truth,
    estimate = !!te$estimate,
    num_breaks = num_breaks,
    conf_level = conf_level,
    include_ribbon = include_ribbon,
    include_rug = include_rug,
    event_level = event_level
  )
}


#' @rdname cal_binary_plot_breaks
#' @export
cal_binary_plot_logistic <- function(.data,
                                     truth,
                                     estimate,
                                     conf_level = 0.90,
                                     include_rug = TRUE,
                                     include_ribbon = TRUE,
                                     event_level = c("first", "second")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_name <- as_name(truth)

  lev <- process_level(event_level)

  truth_levels <- levels(.data[truth_name][[1]])

  if (length(truth_levels) != 2) stop("'", truth_name, "' does not have 2 levels")

  prob_tbl <- cal_binary_table_logistic(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    conf_level = conf_level,
    event_level = event_level
  )

  sub_title <- paste0("'", truth_name, "' is equal to '", truth_levels[[lev]], "'")

  binary_plot_impl(
    prob_tbl, estimate, prob,
    .data, !!truth, !!estimate,
    "Estimate", "Probability",
    sub_title, include_ribbon, include_rug, FALSE
  )
}

#' @rdname cal_binary_plot_breaks
#' @export
cal_binary_plot_windowed <- function(.data,
                                     truth,
                                     estimate,
                                     window_size = round(nrow(.data) / 10),
                                     step_size = window_size,
                                     conf_level = 0.90,
                                     include_ribbon = TRUE,
                                     include_rug = TRUE,
                                     event_level = c("first", "second")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_name <- as_name(truth)

  lev <- process_level(event_level)

  truth_levels <- levels(.data[truth_name][[1]])

  if (length(truth_levels) != 2) stop("'", truth_name, "' does not have 2 levels")

  prob_tbl <- cal_binary_table_windowed(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    window_size = window_size,
    step_size = step_size,
    conf_level = conf_level,
    event_level = event_level
  )

  sub_title <- paste0("'", truth_name, "' is equal to '", truth_levels[[lev]], "'")

  binary_plot_impl(
    prob_tbl, predicted_midpoint, event_rate,
    .data, !!truth, !!estimate,
    "Predicted Midpoint", "Event Rate",
    sub_title, include_ribbon, include_rug, TRUE
  )
}

binary_plot_impl <- function(tbl, x, y, .data, truth, estimate,
                             x_label, y_label, sub_title,
                             include_ribbon, include_rug, include_points) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)


  x <- enquo(x)
  y <- enquo(y)

  res <- ggplot(
    data = tbl,
    aes(x = !!x)
  ) +
    geom_abline(col = "#999999", linetype = 2) +
    geom_line(aes(y = !!y))

  if (include_points) {
    res <- res + geom_point(aes(y = !!y))
  }


  if (include_ribbon) {
    res <- res +
      geom_ribbon(aes(y = !!y, ymin = lower, ymax = upper), alpha = 0.1)
  }

  if (include_rug) {
    level_1_tbl <- dplyr::filter(.data, as.integer(!!truth) == 1)
    level_2_tbl <- dplyr::filter(.data, as.integer(!!truth) != 1)
    res <- res +
      geom_rug(
        data = level_1_tbl,
        aes(x = !!estimate, col = !!truth),
        sides = "b",
        cex = 0.2,
        show.legend = FALSE
      ) +
      geom_rug(
        data = level_2_tbl,
        aes(x = !!estimate, col = !!truth),
        sides = "t",
        cex = 0.2,
        show.legend = FALSE
      )
  }

  sub_title <- paste0("'", as_name(truth), "' vs '", as_name(estimate), "'")

  res +
    tune::coord_obs_pred() +
    labs(
      title = "Calibration Plot",
      subtitle = sub_title,
      x = x_label,
      y = y_label
    ) +
    theme_light()
}

#--------------------------------- Tables --------------------------------------

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
#' probabilities starting at 0, and ending at1.
#' - `cal_binary_table_windowed()` - Creates a running percentage of the probability
#' that moves across the proportion of events.
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
                                    num_breaks = 10,
                                    conf_level = 0.90,
                                    event_level = c("first", "second")) {
  UseMethod("cal_binary_table_breaks")
}

cal_binary_table_breaks_impl <- function(.data,
                                         truth,
                                         estimate,
                                         num_breaks = 10,
                                         conf_level = 0.90,
                                         event_level = c("first", "second")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  bin_exprs <- purrr::map(
    seq_len(num_breaks),
    ~ expr(!!estimate <= !!.x / !!num_breaks ~ !!.x)
  )

  .data %>%
    dplyr::mutate(
      .bin = dplyr::case_when(!!!bin_exprs),
      .is_val = ifelse(as.integer(!!truth) == lev, 1, 0)
    ) %>%
    dplyr::group_by(.bin, .add = TRUE) %>%
    dplyr::summarise(
      predicted_midpoint = median(!!estimate),
      event_rate = sum(.is_val) / dplyr::n(),
      events = sum(.is_val),
      total = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.bin) %>%
    add_conf_intervals(
      events = events,
      total = total,
      conf_level = conf_level
    )
}

#' @export
cal_binary_table_breaks.data.frame <- cal_binary_table_breaks_impl

#' @export
cal_binary_table_breaks.tune_results <- function(.data,
                                                 truth = NULL,
                                                 estimate = NULL,
                                                 num_breaks = 10,
                                                 conf_level = 0.90,
                                                 event_level = c("first", "second")) {
  rs <- tune::collect_predictions(.data, summarize = TRUE)

  te <- tune_results_args(.data, {{ truth }}, {{ estimate }}, event_level, rs)

  cal_binary_table_breaks_impl(
    .data = rs,
    truth = !!te$truth,
    estimate = !!te$estimate,
    num_breaks = num_breaks,
    conf_level = conf_level,
    event_level = event_level
  )
}

#' @rdname cal_binary_table_breaks
#' @export
cal_binary_table_logistic <- function(.data,
                                      truth,
                                      estimate,
                                      conf_level = 0.90,
                                      event_level = c("first", "second")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  prep_data <- dplyr::select(.data, truth = !!truth, estimate = !!estimate)

  model <- mgcv::gam(
    truth ~ s(estimate, k = 10),
    data = prep_data,
    family = binomial()
  )

  new_seq <- seq(0, 1, by = .01)
  new_data <- data.frame(estimate = new_seq)
  preds <- predict(model, new_data, type = "response", se.fit = TRUE)

  res <- tibble::tibble(
    prob = preds$fit,
    se_fit = preds$se.fit
  )

  if (lev == 1) res$prob <- 1 - res$prob

  res <- cbind(new_data, res)

  res$lower <- res$prob - qnorm(conf_level) * res$se_fit
  res$upper <- res$prob + qnorm(conf_level) * res$se_fit
  res$se_fit <- NULL

  tibble::as_tibble(res)
}

#' @rdname cal_binary_table_breaks
#' @export
cal_binary_table_windowed <- function(.data,
                                      truth,
                                      estimate,
                                      window_size = round(nrow(.data) / 10),
                                      step_size = window_size,
                                      conf_level = 0.90,
                                      event_level = c("first", "second")) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  .data %>%
    dplyr::select(!!truth, !!estimate) %>%
    dplyr::arrange(!!estimate) %>%
    slider::slide(~.x,
      .before = window_size,
      .complete = TRUE,
      .step = step_size
    ) %>%
    purrr::map_df(~ {
      if (!is.null(.x)) {
        .x %>%
          dplyr::mutate(
            .is_val = ifelse(as.integer(!!truth) == lev, 1, 0)
          ) %>%
          dplyr::summarise(
            predicted_midpoint = round(median(!!estimate), 4),
            event_rate = round(sum(.is_val) / dplyr::n(), 4),
            events = sum(.is_val),
            total = dplyr::n()
          ) %>%
          add_conf_intervals(events, total, conf_level = conf_level)
      }
    })
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

utils::globalVariables(c(
  ".bin", ".is_val", "event_rate", "events", "lower",
  "predicted_midpoint", "total", "upper"
))

process_level <- function(x) {
  x <- x[[1]]
  ret <- NULL
  if (x == "first") ret <- 1
  if (x == "second") ret <- 2
  if (is.null(ret)) {
    stop("Invalid event_level entry. Valid entries are 'first' and 'second'")
  }
  ret
}

assert_truth_two_levels <- function(.data, truth) {
  truth <- enquo(truth)
  if (!quo_is_null(truth)) {
    truth_name <- as_name(truth)
    truth_levels <- levels(.data[truth_name][[1]])
    if (length(truth_levels) != 2) stop("'", truth_name, "' does not have 2 levels")
  }
}

tune_results_args <- function(.data, truth, estimate, event_level, resampled_data) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if (quo_is_null(truth)) {
    truth_str <- attributes(.data)$outcome
    truth <- parse_expr(truth_str)
  }

  if (quo_is_null(estimate)) {
    truth_str <- as_name(truth)
    lev <- process_level(event_level)
    fc_truth <- levels(resampled_data[[truth_str]])
    estimate_str <- paste0(".pred_", fc_truth[[lev]])
    estimate <- parse_expr(estimate_str)
  }

  list(
    truth = quo(!!truth),
    estimate = quo(!!estimate)
  )
}
