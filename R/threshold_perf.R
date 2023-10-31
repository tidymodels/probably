#' Generate performance metrics across probability thresholds
#'
#' `threshold_perf()` can take a set of class probability predictions
#' and determine performance characteristics across different values
#' of the probability threshold and any existing groups.
#'
#' Note that that the global option `yardstick.event_first` will be
#' used to determine which level is the event of interest. For more details,
#' see the Relevant level section of [yardstick::sens()].
#'
#' The default calculated metrics are:
#' - [yardstick::j_index()]
#' - [yardstick::sens()]
#' - [yardstick::spec()]
#' - `distance = (1 - sens) ^ 2 + (1 - spec) ^ 2`
#'
#' If a custom metric is passed that does not compute sensitivity and
#' specificity, the distance metric is not computed.
#'
#' @param .data A tibble, potentially grouped.
#'
#' @param truth The column identifier for the true two-class results
#' (that is a factor). This should be an unquoted column name.
#'
#' @param estimate The column identifier for the predicted class probabilities
#' (that is a numeric). This should be an unquoted column name.
#'
#' @param ... Currently unused.
#'
#' @param na_rm A single logical: should missing data be removed?
#'
#' @param thresholds A numeric vector of values for the probability
#' threshold. If unspecified, a series
#' of values between 0.5 and 1.0 are used. **Note**: if this
#' argument is used, it must be named.
#'
#' @param metrics Either `NULL` or a [yardstick::metric_set()] with a list of
#' performance metrics to calculate. The metrics should all be oriented towards
#' hard class predictions (e.g. [yardstick::sensitivity()],
#' [yardstick::accuracy()], [yardstick::recall()], etc.) and not
#' class probabilities. A set of default metrics is used when `NULL` (see
#' Details below).
#'
#' @param event_level A single string. Either `"first"` or `"second"` to specify
#' which level of `truth` to consider as the "event".
#'
#' @return A tibble with columns: `.threshold`, `.estimator`, `.metric`,
#' `.estimate` and any existing groups.
#'
#' @examples
#' library(dplyr)
#' data("segment_logistic")
#'
#' # Set the threshold to 0.6
#' # > 0.6 = good
#' # < 0.6 = poor
#' threshold_perf(segment_logistic, Class, .pred_good, thresholds = 0.6)
#'
#' # Set the threshold to multiple values
#' thresholds <- seq(0.5, 0.9, by = 0.1)
#'
#' segment_logistic %>%
#'   threshold_perf(Class, .pred_good, thresholds)
#'
#' # ---------------------------------------------------------------------------
#'
#' # It works with grouped data frames as well
#' # Let's mock some resampled data
#' resamples <- 5
#'
#' mock_resamples <- resamples %>%
#'   replicate(
#'     expr = sample_n(segment_logistic, 100, replace = TRUE),
#'     simplify = FALSE
#'   ) %>%
#'   bind_rows(.id = "resample")
#'
#' resampled_threshold_perf <- mock_resamples %>%
#'   group_by(resample) %>%
#'   threshold_perf(Class, .pred_good, thresholds)
#'
#' resampled_threshold_perf
#'
#' # Average over the resamples
#' resampled_threshold_perf %>%
#'   group_by(.metric, .threshold) %>%
#'   summarise(.estimate = mean(.estimate))
#'
#' @export
threshold_perf <- function(.data, ...) {
  UseMethod("threshold_perf")
}

#' @rdname threshold_perf
#' @export
threshold_perf.data.frame <- function(.data,
                                      truth,
                                      estimate,
                                      thresholds = NULL,
                                      metrics = NULL,
                                      na_rm = TRUE,
                                      event_level = "first",
                                      ...) {
  if (is.null(thresholds)) {
    thresholds <- seq(0.5, 1, length = 21)
  }
  if (is.null(metrics)) {
    metrics <-
      yardstick::metric_set(
        yardstick::sensitivity,
        yardstick::specificity,
        yardstick::j_index
      )
  }
  measure_sens_spec <- check_thresholded_metrics(metrics)

  obs_sel <- tidyselect::eval_select(
    expr = enquo(truth),
    data = .data
  )
  probs_sel <- tidyselect::eval_select(
    expr = enquo(estimate),
    data = .data
  )

  obs <- names(obs_sel)
  probs <- names(probs_sel)

  rs_ch <- dplyr::group_vars(.data)
  rs_ch <- unname(rs_ch)

  obs_sym <- sym(obs)
  probs_sym <- sym(probs)

  if (length(rs_ch) == 0) {
    rs_ch <- NULL
    rs_id <- NULL
  } else {
    rs_id <- syms(rs_ch)
  }

  if (length(probs) > 1 | length(obs) > 1) {
    cli::cli_abort(
      "{.arg truth} and {.arg estimate} should only be single columns."
    )
  }
  if (!inherits(.data[[obs]], "factor")) {
    cli::cli_abort("{.arg truth} should be a factor.")
  }

  if (length(levels(.data[[obs]])) != 2) {
    cli::cli_abort("{.arg truth} should be a 2 level factor.")
  }
  if (!is.numeric(.data[[probs]])) {
    cli::cli_abort("{.arg estimate} should be numeric.")
  }

  .data <- dplyr::rename(.data, truth = !!obs_sym, prob = !!probs_sym)

  if (!is.null(rs_id)) {
    .data <- dplyr::select(.data, truth, prob, !!!rs_id)
  } else {
    .data <- dplyr::select(.data, truth, prob)
  }

  if (na_rm) {
    .data <- stats::na.omit(.data)
  }

  .data <- .data %>%
    expand_preds(
      threshold = thresholds,
      inc = c("truth", "prob", rs_ch)
    ) %>%
    dplyr::mutate(
      alt_pred = recode_data(
        obs = truth,
        prob = prob,
        threshold = .threshold,
        event_level = event_level
      )
    )

  if (!is.null(rs_id)) {
    .data <- .data %>% dplyr::group_by(!!!rs_id, .threshold)
  } else {
    .data <- .data %>% dplyr::group_by(.threshold)
  }


  .data_metrics <- metrics(
    .data,
    truth = truth,
    estimate = alt_pred,
    event_level = event_level
  )

  if (measure_sens_spec) {
    # Create the `distance` metric data frame
    # and add it on
    sens_vec <- .data_metrics %>%
      dplyr::filter(.metric == "sens") %>%
      dplyr::pull(.estimate)

    dist <- .data_metrics %>%
      dplyr::filter(.metric == "spec") %>%
      dplyr::mutate(
        .metric = "distance",
        # .estimate is specificity currently. This recodes as distance
        .estimate = (1 - sens_vec)^2 + (1 - .estimate)^2
      )

    .data_metrics <- dplyr::bind_rows(.data_metrics, dist)
  }

  .data_metrics
}

expand_preds <- function(.data, threshold, inc = NULL) {
  threshold <- unique(threshold)
  nth <- length(threshold)
  n_data <- nrow(.data)
  if (!is.null(inc)) {
    .data <- dplyr::select(.data, tidyselect::all_of(inc))
  }
  .data <- .data[rep(1:nrow(.data), times = nth), ]
  .data$.threshold <- rep(threshold, each = n_data)
  .data
}


check_thresholded_metrics <- function(x) {
  y <- dplyr::as_tibble(x)
  if (!all(y$class == "class_metric")) {
    rlang::abort("All metrics must be of type 'class_metric' (e.g. `sensitivity()`, ect)")
  }
  # check to see if sensitivity and specificity are in the lists
  has_sens <-
    any(y$metric %in% c("sens", "sensitivity")) &
      any(y$metric %in% c("spec", "specificity"))
  has_sens
}

utils::globalVariables(
  c(
    ".",
    ".threshold",
    "alt_pred",
    "prob",
    "statistic",
    "value",
    ".metric",
    ".estimate",
    "distance"
  )
)
