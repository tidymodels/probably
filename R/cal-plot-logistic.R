#----------------------------- Plot methods ------------------------------------

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
                              group = NULL,
                              conf_level = 0.90,
                              smooth = TRUE,
                              include_rug = TRUE,
                              include_ribbon = TRUE,
                              event_level = c("auto", "first", "second"),
                              ...) {
  UseMethod("cal_plot_logistic")
}


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
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

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
    include_points = FALSE,
    is_tune_results = is_tune_results
  )
}

#' @export
#' @rdname cal_plot_logistic
cal_plot_logistic.data.frame <- cal_plot_logistic_impl

#' @export
#' @rdname cal_plot_logistic
cal_plot_logistic.tune_results <- function(.data,
                                           truth = NULL,
                                           estimate = dplyr::starts_with(".pred"),
                                           group = NULL,
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
    smooth = smooth,
    is_tune_results = TRUE
  )
}

#---------------------------- Table methods ------------------------------------


#' @rdname cal_binary_tables
#' @export
#' @keywords internal
.cal_binary_table_logistic <- function(.data,
                                       truth = NULL,
                                       estimate = NULL,
                                       group = NULL,
                                       conf_level = 0.90,
                                       event_level = c("auto", "first", "second"),
                                       ...) {
  UseMethod(".cal_binary_table_logistic")
}

.cal_binary_table_logistic_impl <- function(.data,
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

  res <- .data %>%
    dplyr::group_by(!!group, .add = TRUE) %>%
    dplyr::group_map(~ {
      grp <- .cal_class_grps(
        .data = .x,
        truth = !!truth,
        levels = levels,
        conf_level = conf_level,
        event_level = event_level,
        smooth = smooth,
        method = "model"
      )
      dplyr::bind_cols(.y, grp)
    }) %>%
    dplyr::bind_rows()

  if (length(levels) > 2) {
    res <- dplyr::group_by(res, !!truth)
  }

  res
}

.cal_binary_table_logistic_grp <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           conf_level = 0.90,
                                           event_level = c("auto", "first", "second"),
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

  res <- dplyr::tibble(
    prob = binomial()$linkinv(preds$fit),
    lower = binomial()$linkinv(preds$fit - qnorm(conf_level) * preds$se.fit),
    upper = binomial()$linkinv(preds$fit + qnorm(conf_level) * preds$se.fit)
  )

  res <- cbind(new_data, res)

  dplyr::as_tibble(res)
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

  .cal_binary_table_logistic_impl(
    .data = tune_args$predictions,
    truth = !!tune_args$truth,
    estimate = !!tune_args$estimate,
    group = !!tune_args$group,
    conf_level = conf_level,
    event_level = event_level
  )
}
