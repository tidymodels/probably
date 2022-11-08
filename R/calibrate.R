#' Probability Calibration plots
#'
#' @description Calibration plot functions. They require a data.frame that contains
#' the predictions and probability columns. The output is a `ggplot2` graph.
#'
#' @param .data A data.frame object containing predictions and probability columns
#' @param truth The column identifier for the true class results
#' (that is a factor). This should be an unquoted column name
#' @param estimate The column identifier for the prediction probabilities.
#' This should be an unquoted column name
#' @param num_breaks The number of segments to group the probabilities. Defaults
#' to 10
#' @param conf_level Confidence level to use in the visualization. Defaults to 0.9
#' @param include_ribbon Flag that indicates if the ribbon layer is to be
#' included. Defaults to `TRUE`
#' @param include_rug Flag that indicates if the Rug layer is to be included.
#' Defaults to `TRUE`
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
#' @export
cal_binary_plot_breaks <- function(.data,
                                   truth,
                                   estimate,
                                   num_breaks = 10,
                                   conf_level = 0.90,
                                   include_ribbon = TRUE,
                                   include_rug = TRUE
                                   ) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_name <- as_name(truth)

  truth_levels <- levels(.data[truth_name][[1]])

  if(length(truth_levels) != 2) stop("'", truth_name, "' does not have 2 levels")

  prob_tbl <- cal_binary_table_breaks(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    num_breaks = num_breaks,
    conf_level = conf_level
  )

  sub_title <- paste0("'", truth_name, "' is equal to '", truth_levels[[1]], "'")

  binary_plot_impl(prob_tbl, predicted_midpoint, event_rate,
                   .data, !!truth, !!estimate,
                   "Predicted Midpoint", "Event Rate",
                   sub_title, include_ribbon, include_rug, TRUE
                   )
}

#' @rdname cal_binary_plot_breaks
#' @export
cal_binary_plot_logistic <- function(.data,
                                     truth,
                                     estimate,
                                     conf_level = 0.90,
                                     include_rug = TRUE,
                                     include_ribbon = TRUE
                                     ) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_name <- as_name(truth)

  truth_levels <- levels(.data[truth_name][[1]])

  if(length(truth_levels) != 2) stop("'", truth_name, "' does not have 2 levels")

  prob_tbl <- cal_binary_table_logistic(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    conf_level = conf_level
  )

  sub_title <- paste0("'", truth_name, "' is equal to '", truth_levels[[1]], "'")

  binary_plot_impl(prob_tbl, estimate, prob,
                   .data, !!truth, !!estimate,
                   "Estimate", "Probability",
                   sub_title, include_ribbon, include_rug, FALSE
                   )
}

binary_plot_impl <- function(tbl, x, y, .data, truth, estimate,
                             x_label, y_label, sub_title,
                             include_ribbon, include_rug, include_points
) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  x  <- enquo(x)
  y <- enquo(y)

  res <- ggplot(
    data = tbl,
    aes(x = !!x)
  ) +
    geom_abline(col = "#999999", linetype = 2) +
    geom_line(aes(y = !!y))

  if(include_points) {
    res <- res + geom_point(aes(y = !!y))
  }


  if(include_ribbon) {
    res <- res +
      geom_ribbon(aes(y = !!y, ymin = lower, ymax = upper), alpha = 0.1)
  }

  if(include_rug) {
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

  res +
    tune::coord_obs_pred() +
    labs(
      title = "Calibration Plot",
      subtitle = sub_title,
      x = x_label,
      y = y_label
    )
}


#' Probability Calibration table
#'
#' @description Calibration table functions. They require a data.frame that
#' contains the predictions and probability columns. The output is another
#' `tibble` with segmented data that compares the accuracy of the probability
#' to the actual outcome.
#'
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
#' @export
cal_binary_table_breaks <- function(.data,
                                   truth,
                                   estimate,
                                   num_breaks = 10,
                                   conf_level = 0.90
                                   ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  bin_exprs <- purrr::map(
    seq_len(num_breaks),
    ~ expr(!!estimate <= !!.x / !!num_breaks ~ !!.x)
  )

  bin_data <- .data %>%
    dplyr::mutate(
      .bin = dplyr::case_when(!!!bin_exprs),
      .is_val = ifelse(as.integer(!!truth) == 1, 1, 0)
    ) %>%
    dplyr::group_by(.bin, .add = TRUE) %>%
    dplyr::summarise(
      predicted_midpoint = median(!!estimate),
      event_rate = sum(.is_val) / dplyr::n(),
      events = sum(.is_val),
      total = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.bin)

  add_conf_intervals(
    .data = bin_data,
    events = events,
    total = total,
    conf_level = conf_level
    )
}

add_conf_intervals <- function(.data,
                               events = events,
                               total = total,
                               conf_level = 0.90
                               ) {
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

#' @rdname cal_binary_table_breaks
#' @export
cal_binary_table_logistic <- function(.data,
                                     truth,
                                     estimate,
                                     conf_level = 0.90
                                     ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

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
    prob = 1 - preds$fit,
    se_fit = preds$se.fit
  )

  res <- cbind(new_data, res)

  res$lower <- res$prob - qnorm(conf_level) * res$se_fit
  res$upper <- res$prob + qnorm(conf_level) * res$se_fit
  res$se_fit <- NULL

  tibble::as_tibble(res)

}

utils::globalVariables(c(
  ".bin", ".is_val", "event_rate", "events", "lower",
  "predicted_midpoint", "total", "upper"
))
