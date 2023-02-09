#' Determine the best calibration method using resampling
#'
#' @inheritParams cal_validate_logistic
#' @param summarize Should the overall ranks be computed (the default) or should
#' the resample-level results be returned?
#' @param level Confidence level for two-sided intervals produced by
#' [stats::t.test()] (`summarize = TRUE` only)
#'
#' @export
cal_rank <-
  function(.data, truth = NULL, estimate = dplyr::starts_with(".pred_"),
           metrics = NULL, ...) {
  UseMethod("cal_rank")
  }

# TODO make a function to go between class pred (.pred_) and reg pred (.pred) in starts_wi

#' @export
#' @rdname cal_rank
cal_rank.resample_results <-
  function(.data, truth = NULL, estimate = dplyr::starts_with(".pred_"),
           metrics = NULL, summarize = TRUE, level = 0.90, ...) {
    truth <- enquo(truth)
    estimate <- enquo(estimate)
    res <- opt_binary(.data,
                      truth = truth,
                      estimate = estimate,
                      metrics = metrics,
                      ...)

    if (summarize) {
      res <- summary_rank(res, level = level)
      class(res) <- c("cal_rank_summary", "cal_rank", class(res))
    } else {
      class(res) <- c("cal_rank", class(res))
    }
    res
  }

# @export
# @rdname cal_rank
# cal_rank.rset <-
#   function(.data, truth = NULL, estimate = dplyr::starts_with(".pred_"),
#            metrics = NULL, summarize = TRUE, level = 0.90, ...) {
#     truth <- enquo(truth)
#     estimate <- enquo(estimate)
#     res <- opt_binary(.data,
#                       truth = truth,
#                       estimate = estimate,
#                       metrics = metrics,
#                       ...)
#     if (summarize) {
#       res <- summary_rank(res, level = level)
#       class(res) <- c("cal_rank_summary", "cal_rank", class(res))
#     } else {
#       class(res) <- c("cal_rank", class(res))
#     }
#     res
#   }

# ------------------------------------------------------------------------------

# TODO use call2 and purrr::map to make this more elegant

opt_binary <- function(object, truth, estimate, metrics, ...) {

  res <-
    dplyr::bind_rows(
      get_improvement(
        object, "logistic", {{truth}}, {{estimate}}, metrics, ...
      ) %>%
        dplyr::mutate(method = "logistic (smoothed)"),
      get_improvement(
        object, "logistic", {{truth}}, {{estimate}}, metrics, smooth = FALSE, ...
      ),
      get_improvement(
        object, "beta", {{truth}}, {{estimate}}, metrics, ...
      ),
      get_improvement(
        object, "isotonic", {{truth}}, {{estimate}}, metrics, ...
      ),
      get_improvement
      (object, "isotonic_boot", {{truth}}, {{estimate}}, metrics, ...
      )
    ) %>%
    dplyr::relocate(method)
  res
}

opt_reg <- function(object, ...) {

  res <-
    dplyr::bind_rows(
      get_improvement(object, "linear", ...) %>%
        dplyr::mutate(method = "linear (smoothed)"),
      get_improvement(object, "linear", smooth = FALSE, ...)
      # get_improvement(object, "isotonic", ...),
      # get_improvement(object, "isotonic_boot", ...)
    ) %>%
    dplyr::relocate(method)
  res
}

# ------------------------------------------------------------------------------

get_improvement <- function(object, method, truth, estimate, metrics, ...) {

  fn <- paste0("cal_validate_", method)
  cl <-
    rlang::call2(
      .fn = fn,
      .data = rlang::expr(object),
      truth = truth,
      estimate = estimate,
      metrics = rlang::expr(metrics),
      summarize = FALSE,
      ...
    )
  results <- rlang::eval_tidy(cl)
  id_cols <- grep("^id", names(object), value = TRUE)
  join_cols <- c(id_cols, ".metric", ".estimator", "direction")

  res_uncal <-
    results %>%
    dplyr::select(dplyr::starts_with("id"), stats_before) %>%
    tidyr::unnest(cols = "stats_before") %>%
    dplyr::rename(original = .estimate)
  res_cal <-
    results %>%
    dplyr::select(dplyr::starts_with("id"), stats_after) %>%
    tidyr::unnest(cols = "stats_after") %>%
    dplyr::rename(calibrated = .estimate)


  results <-
    dplyr::full_join(res_uncal, res_cal, by = join_cols) %>%
    dplyr::mutate(
      improvement = ifelse(direction == "minimize", original - calibrated, calibrated - original),
      method = method
    )
  results
}


# print.cal_rank <- function(x, digits = 3, ...) {
#
#   dplyr::group_by(x, method, .metric) %>%
#     dplyr::summarize(
#       `mean improvement` = mean(improvement, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     dplyr::mutate(
#       `mean improvement` = signif(`mean improvement`, digits = digits)
#     ) %>%
#     print()
# }

broomish_t <- function(x, level = 0.9) {
  ttest <- t.test(x$improvement, conf.level = level)
  dplyr::tibble(
    improvement = as.vector(ttest$estimate),
    p_value = ttest$p.value,
    conf_lower = ttest$conf.int[1],
    conf_upper = ttest$conf.int[2]
  )
}

summary_rank <- function(x, level = 0.90, ...) {
 x %>%
    tidyr::nest(data = improvement, .by = c(.metric, direction, method)) %>%
    dplyr::mutate(
      stats = purrr::map(data, ~ broomish_t(.x, level = level))
    ) %>%
    tidyr::unnest(stats) %>%
    dplyr::select(-direction, -data) %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.double), ~ ifelse(is.nan(.x), NA_real_, .x))
    ) %>%
    dplyr::arrange(.metric, dplyr::desc(improvement))
}

#' @export
autoplot.cal_rank <- function(x, level = 0.90, metrics = NULL, ...) {
  if (!inherits(x, "cal_rank_summary")) {
    x <- summary_rank(x, level = level)
  }

  if (!is.null(metrics)) {
    x <- dplyr::filter(x, .metric %in% metrics)
  }
  n_method <- length(unique(x$method))
  n_metrics <- length(unique(x$.metric))

  x <- dplyr::mutate(x, method = stats::reorder(method, improvement))

  p <-
    ggplot2::ggplot(
      data = x,
      ggplot2::aes(y = method)
    ) +
    ggplot2::geom_vline(xintercept = 0, col = "green", lty = 2) +
    ggplot2::geom_point(
      ggplot2::aes(x = improvement)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = conf_lower, xmax = conf_upper),
      width = n_method / 15
    )
  if (n_method == 1) {
    p <- p +
      ggplot2::labs(y = NULL, x = "Improvement")
  } else {
    p <- p +
      ggplot2::labs(y = NULL, x = "Improvement") +
      ggplot2::facet_wrap(~ .metric, scales = "free_x")
  }
  p
}



