#' Obtain and validate performance metrics from calibration
#' @param metrics A set of metrics passed created via `yardstick::metric_set()`
#' @param summarize Indicates to pass tibble with the metrics averaged, or
#' if to return the same sampled object but with new columns containing the
#' calibration y validation list columns.
#' @inheritParams cal_estimate_logistic
#' @export
cal_validate_logistic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  smooth = TRUE,
                                  parameters = NULL,
                                  metrics = NULL,
                                  summarize = TRUE,
                                  ...) {
  UseMethod("cal_validate_logistic")
}

#' @export
cal_validate_logistic.rset <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  smooth = TRUE,
                                  parameters = NULL,
                                  metrics = NULL,
                                  summarize = TRUE,
                                  ...) {
  cal_validate(
    rset = .data,
    truth = {{truth}},
    estimate = {{estimate}},
    cal_function = "logistic",
    metrics = metrics,
    summarize = summarize,
    smooth = smooth,
    parameters = parameters,
    ...
  )
}

#' @export
cal_validate_isotonic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  parameters = NULL,
                                  metrics = NULL,
                                  summarize = TRUE,
                                  ...) {
  UseMethod("cal_validate_isotonic")
}

#' @export
cal_validate_isotonic.rset <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  parameters = NULL,
                                  metrics = NULL,
                                  summarize = TRUE,
                                  ...) {
  cal_validate(
    rset = .data,
    truth = {{truth}},
    estimate = {{estimate}},
    cal_function = "isotonic",
    metrics = metrics,
    summarize = summarize,
    ...
  )
}




cal_validate <- function(rset,
                         truth = NULL,
                         estimate = NULL,
                         cal_function = NULL,
                         metrics = NULL,
                         summarize = TRUE,
                         ...
                         ) {
  if(is.null(cal_function)) rlang::abort("No calibration function provided")

  if (is.null(metrics)) {
    metrics <- yardstick::metric_set(
      yardstick::brier_class,
      yardstick::roc_auc
      )
  }

  data_tr <- map(rset$splits, rsample::training)
  data_as <- map(rset$splits, rsample::assessment)

  if(cal_function == "logistic") {
    cals <- map(
      data_tr,
      cal_estimate_logistic,
      truth = {{ truth }},
      estimate = {{estimate}},
      ...
      )
  }

  if(cal_function == "isotonic") {
    cals <- map(
      data_tr,
      cal_estimate_isotonic,
      truth = {{ truth }},
      estimate = {{estimate}},
      ...
    )
  }

  estimate_col <- cals[[1]]$levels[[1]]

  applied <- map(
    seq_along(data_as),
    ~ {
      val <- cal_apply(data_as[[.x]], cals[[.x]])
      stats_after <- metrics(val, truth = {{truth}}, estimate_col)
      stats_before <- metrics(data_as[[.x]], truth = {{truth}}, estimate_col)

      list(
        val = val,
        stats_after = stats_after,
        stats_before = stats_before
      )
    }) %>%
    purrr::transpose()

  x <- dplyr::mutate(
    rset,
    calibration = cals,
    validation = applied$val,
    stats_after = applied$stats_after,
    stats_before = applied$stats_before
  )

  if(summarize) x <- summarize_validation(x)

   x
}

#' @importFrom pillar type_sum
#' @export
type_sum.cal_binary <- function(x, ...) {
  paste0(x$method, " [", x$rows, "]")
}


summarize_validation <- function(x) {

  fs <- x$stats_after[[1]]

  fs$.estimate <- NULL

  seq_len(nrow(fs)) %>%
    map(~ {
      y <- .x
      sa <- purrr::map_dbl(x$stats_after, ~ .x[y, ]$.estimate)
      mean_sa <- mean(sa)
      sb <- purrr::map_dbl(x$stats_before, ~ .x[y, ]$.estimate)
      mean_sb <- mean(sb)
      ret <- fs[y, ]
      ret$.before <- mean_sb
      ret$.after <- mean_sa
      ret
    }) %>%
    dplyr::bind_rows()
}
