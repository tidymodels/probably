#' Obtain and validate performance metrics from calibration
#' @param metrics A set of metrics passed created via `yardstick::metric_set()`
#' @inheritParams cal_estimate_logistic
#' @export
cal_validate_logistic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  smooth = TRUE,
                                  parameters = NULL,
                                  metrics = NULL,
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
                                  ...) {
  cal_validate(
    rset = .data,
    truth = {{truth}},
    estimate = {{estimate}},
    cal_function = "logistic",
    metrics = metrics,
    ...
  )
}



cal_validate <- function(rset,
                         truth = NULL,
                         estimate = NULL,
                         cal_function = NULL,
                         metrics = NULL,
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
    })

  dplyr::mutate(
    rset,
    estimate = cals,
    validation = map(applied, ~.x$val),
    stats_after = map(applied, ~.x$stats_after),
    stats_before = map(applied, ~.x$stats_before)
  )
}
