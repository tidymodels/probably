# ------------------------------- Methods --------------------------------------

cal_apply_binary <- function(object, .data, pred_class) {
  UseMethod("cal_apply_binary")
}

cal_apply_binary.cal_estimate_logistic <- function(object,
                                                   .data,
                                                   pred_class = NULL,
                                                   ...) {
  apply_model_predict(
    object = object,
    .data = .data
  )
}

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

apply_model_predict <- function(object, .data) {
  if (object$type == "binary") {
    .data <- object$estimates %>%
      purrr::map(
        ~ {
          if (is.null(.x$filter)) {
            new_data <- .data
          } else {
            new_data <- dplyr::filter(.data, !!.x$filter)
          }
          preds <- predict(.x$estimate, newdata = new_data, type = "response")
          preds <- 1 - preds
          new_data[object$levels[[1]]] <- preds
          new_data[object$levels[[2]]] <- 1 - preds
          new_data
        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)
  }
  .data
}
