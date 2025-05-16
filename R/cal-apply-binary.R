# ------------------------------- Methods --------------------------------------

cal_apply_binary <- function(object, .data, pred_class) {
  UseMethod("cal_apply_binary")
}

#' @export
cal_apply_binary.cal_estimate_logistic <- function(object,
                                                   .data,
                                                   pred_class = NULL,
                                                   ...) {
  apply_model_predict(
    object = object,
    .data = .data
  )
}

#' @export
cal_apply_binary.cal_estimate_logistic_spline <- function(object,
                                                          .data,
                                                          pred_class = NULL,
                                                          ...) {
  apply_model_predict(
    object = object,
    .data = .data
  )
}

#---------------------------- Adjust implementations ---------------------------

binary_repredict <- function(x, predict_data, object) {
  if (is.null(x$filter)) {
    new_data <- predict_data
  } else {
    new_data <- dplyr::filter(predict_data, !!x$filter)
  }
  preds <- predict(x$estimate, newdata = new_data, type = "response")
  preds <- 1 - preds
  lvls <- nm_levels(object$levels)
  new_data[lvls[1]] <- preds
  new_data[lvls[2]] <- 1 - preds
  new_data
}

apply_model_predict <- function(object, .data) {
  if (object$type == "binary") {
    .data <-
      purrr::map(
        object$estimates,
        binary_repredict,
        predict_data = .data,
        object = object
      ) |>
      purrr::reduce(dplyr::bind_rows)
  }
  .data
}
