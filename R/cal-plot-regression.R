#' Regression calibration plots
#'
#' @description
#' A scatter plot of the observed and predicted values is computed where the
#' axes are the same. When `smooth = TRUE`, a generalized additive model fit
#' is shown. If the predictions are well calibrated, the fitted curve should align with
#' the diagonal line.
#'
#' @param .data An ungrouped data frame object containing a prediction
#' column.
#' @param truth The column identifier for the true results
#' (numeric). This should be an unquoted column name.
#' @param estimate The column identifier for the predictions.
#' This should be an unquoted column name
#' @param .by The column identifier for the grouping variable. This should be
#' a single unquoted column name that selects a qualitative variable for
#' grouping. Default to `NULL`. When `.by = NULL` no grouping will take place.
#' @param smooth A logical: should a smoother curve be added.
#' @param ... Additional arguments passed to [ggplot2::geom_point()].
#' @return A ggplot object.
#' @examples
#' cal_plot_regression(boosting_predictions_oob, outcome, .pred)
#'
#' cal_plot_regression(boosting_predictions_oob, outcome, .pred,
#'   alpha = 1 / 6, cex = 3, smooth = FALSE
#' )
#'
#' cal_plot_regression(boosting_predictions_oob, outcome, .pred,
#'   .by = id,
#'   alpha = 1 / 6, cex = 3, smooth = FALSE
#' )
#' @export
cal_plot_regression <- function(.data,
                                truth = NULL,
                                estimate = NULL,
                                smooth = TRUE,
                                ...) {
  UseMethod("cal_plot_regression")
}

cal_plot_regression_impl <- function(.data,
                                     truth = NULL,
                                     estimate = NULL,
                                     smooth = TRUE,
                                     ...,
                                     .by = NULL) {
  group <- get_group_argument({{ .by }}, .data)

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  assert_truth_numeric(.data, !!truth)

  regression_plot_impl(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    group = !!group,
    smooth = smooth,
    ...
  )
}

#' @export
#' @rdname cal_plot_regression
cal_plot_regression.data.frame <- cal_plot_regression_impl

#' @export
#' @rdname cal_plot_regression
cal_plot_regression.tune_results <- function(.data,
                                             truth = NULL,
                                             estimate = NULL,
                                             smooth = TRUE,
                                             ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    ...
  )

  cal_plot_regression_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    .by = !!tune_args$group,
    smooth = smooth,
    ...
  )
}

#' @export
#' @rdname cal_plot_regression
cal_plot_regression.grouped_df <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           smooth = TRUE,
                                           ...) {
  abort_if_grouped_df()
}

regression_plot_impl <- function(.data, truth, estimate, group,
                                 smooth, ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  if (quo_is_null(group)) {
    .data[[".config"]] <- NULL
  }

  gp_vars <- dplyr::group_vars(.data)

  if (length(gp_vars)) {
    if (length(gp_vars) > 1) {
      cli::cli_abort("Plot does not support more than one grouping variable")
    }
    has_groups <- TRUE
    dplyr_group <- parse_expr(gp_vars)
    grouping_var <- tbl[, gp_vars][[1]]
    if (is.numeric(grouping_var)) {
      tbl[, gp_vars] <- as.factor(format(grouping_var))
    }
  } else {
    has_groups <- FALSE
    dplyr_group <- NULL
  }

  res <-
    ggplot(
      data = .data,
      aes(
        x = !!quo_to_sym(truth, .data),
        y = !!quo_to_sym(estimate, .data),
        color = !!quo_to_sym(dplyr_group, .data),
        fill = !!quo_to_sym(dplyr_group, .data)
      )
    ) +
    geom_abline(col = "green", linetype = 2) +
    geom_point(...) +
    tune::coord_obs_pred(ratio = 1) +
    labs(x = "Observed", y = "Predicted")

  if (smooth) {
    res <-
      res + geom_smooth(
        se = FALSE,
        col = "blue",
        method = "gam",
        formula = y ~ s(x, bs = "cs")
      )
  } else {
    res <-
      res + geom_smooth(
        se = FALSE,
        col = "blue",
        method = "lm",
        formula = y ~ x
      )
  }

  if (!quo_is_null(group)) {
    res <- res + facet_wrap(group)
  }

  res
}



assert_truth_numeric <- function(.data, truth) {
  truth <- enquo(truth)
  if (!quo_is_null(truth)) {
    truth_name <- as_name(truth)
    y <- .data[[truth_name]]

    if (!is.numeric(y)) {
      cli::cli_abort("{.val {truth_name}}' should be a numeric vector.")
    }
  }
}
