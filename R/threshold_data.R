#' Generate data for thresholding two-class data
#'
#' `threshold_data` can take a set of class probability predictions
#'  and determine performance characteristics across different values
#'  of the probability threshold and any existing groups.
#'
#' @param .data A tibble or data frame, potentially grouped.
#'
#' @param truth A two-level factor of the true outcome labels.
#'  Note that that the global option `yardstick.event_first` will be
#'  used to determine which level is the event of interest.
#'
#' @param estimate A numeric vector of class probabilities such
#'  that larger values are more indicative of the event of interest.
#'
#' @param ... Currently unused.
#'
#' @param na_rm A single logical: should missing data be removed?
#'
#' @param thresholds A numeric vector of values for the probability
#'  threshold to call a sample an event. If unspecified, a series
#'  of values between 0.5 and 1.0 are used. **Note**: if this
#'  argument is used, it must be named.
#'
#' @return A tibble with columns for the performance
#'  characteristics, the threshold, and existing groups.
#'
#' @export
threshold_data <- function(.data, ...) {
  UseMethod("threshold_data")
}

#' @rdname threshold_data
#' @importFrom tidyselect vars_select
#' @importFrom dplyr rename select mutate group_by do summarise
#' @importFrom dplyr %>% tibble ungroup
#' @importFrom tidyr gather spread
#' @importFrom stats na.omit
#' @export
threshold_data.data.frame <- function(.data,
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

  obs <- sym(obs)
  probs <- sym(probs)
  if (length(rs_ch) == 0) {

    rs_ch <- NULL
    rs_id <- NULL

  } else {

    if (length(rs_ch) > 1)
      rs_id <- syms(rs_ch)
    else
      rs_id <- sym(rs_ch)

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

  .data <- dplyr::rename(.data, truth = !!obs, prob = !!probs)

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

  .data <- .data %>%
    two_class(truth, alt_pred) %>%
    spread(.metric, .estimate) %>%
    mutate(distance = (1 - sens) ^ 2 + (1 - spec) ^ 2) %>%
    gather(key = ".metric", ".estimate", j_index, sens, spec, distance) %>%
    ungroup()

  .data
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

