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
#' The currently calculated metrics are:
#' - [yardstick::j_index()]
#' - [yardstick::sens()]
#' - [yardstick::spec()]
#' - `distance = (1 - sens) ^ 2 + (1 - spec) ^ 2`
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
#' @importFrom tidyselect vars_select
#' @importFrom dplyr rename select mutate group_by do summarise
#' @importFrom dplyr %>% tibble ungroup
#' @importFrom stats na.omit
#' @export
threshold_perf.data.frame <- function(.data,
                                      truth,
                                      estimate,
                                      thresholds = NULL,
                                      na_rm = TRUE,
                                      ...) {

  if (is.null(thresholds)) {
    thresholds <- seq(0.5, 1, length = 21)
  }

  nms   <- names(.data)
  obs   <- tidyselect::vars_select(nms, !!enquo(truth))
  probs <- tidyselect::vars_select(nms, !!enquo(estimate))
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

  if (length(probs) > 1 | length(obs) > 1)
    stop("`truth` and `estimate` should only be single columns.",
         call. = FALSE)
  if (!inherits(.data[[obs]], "factor"))
    stop("`truth` should be a factor", call. = FALSE)
  if (length(levels(.data[[obs]])) != 2)
    stop("`truth` should be a 2 level factor", call. = FALSE)
  if (!is.numeric(.data[[probs]]))
    stop("`estimate` should be numericr", call. = FALSE)

  .data <- dplyr::rename(.data, truth = !!obs_sym, prob = !!probs_sym)

  if (!is.null(rs_id)) {
    .data <- dplyr::select(.data, truth, prob, !!!rs_id)
  } else {
    .data <- dplyr::select(.data, truth, prob)
  }

  if (na_rm) {
    .data <- na.omit(.data)
  }

  .data <- .data %>%
    expand_preds(
      threshold = thresholds,
      inc = c("truth", "prob", rs_ch)
    ) %>%
    mutate(
      alt_pred = recode_data(truth, prob, .threshold)
    )

  if (!is.null(rs_id)) {
    .data <- .data %>% group_by(!!!rs_id, .threshold)
  } else {
    .data <- .data %>% group_by(.threshold)
  }

  .data_metrics <- .data %>%
    two_class(truth, estimate = alt_pred)

  # Create the `distance` metric data frame
  # and add it on
  sens_vec <- .data_metrics %>%
    dplyr::filter(.metric == "sens") %>%
    dplyr::pull(.estimate)

  dist <- .data_metrics %>%
    dplyr::filter(.metric == "spec") %>%
    dplyr::mutate(
      .metric = "distance",
      # .estimate is spec currently. this recodes as distance
      .estimate = (1 - sens_vec) ^ 2 + (1 - .estimate) ^ 2
    )

  .data_metrics <- dplyr::bind_rows(.data_metrics, dist)

  .data_metrics
}

#' @importFrom yardstick sens spec j_index metric_set
two_class <- function(...) {
  mets <- metric_set(sens, spec, j_index)
  mets(...)
}

expand_preds <- function(.data, threshold, inc = NULL) {
  threshold <- unique(threshold)
  nth <- length(threshold)
  n_data <- nrow(.data)
  if (!is.null(inc))
    .data <- dplyr::select(.data, inc)
  .data <- .data[rep(1:nrow(.data), times = nth), ]
  .data$.threshold <- rep(threshold, each = n_data)
  .data
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

