# ------------------------------- Methods --------------------------------------

cal_apply_regression <- function(object, .data, pred_class) {
  UseMethod("cal_apply_regression")
}

cal_apply_regression.cal_estimate_linear_spline <-
  function(object, .data, pred_class = NULL, ...) {
    apply_reg_predict(
      object = object,
      .data = .data
    )
  }

cal_apply_regression.cal_estimate_linear <-
  cal_apply_regression.cal_estimate_linear_spline

cal_apply_regression.cal_estimate_isotonic <- function(object,
                                                       .data,
                                                       pred_classs = NULL,
                                                       ...) {
  apply_interval_impl(
    object = object,
    .data = .data,
    multi = FALSE
    )
}

cal_apply_regression.cal_estimate_isotonic_boot <-
  cal_apply_regression.cal_estimate_isotonic

#---------------------------- Adjust implementations ---------------------------

apply_reg_predict <- function(object, .data) {

  .pred_name <- rlang::expr_deparse(object$levels$predictions)
  .data <- object$estimates %>%
    purrr::map(
      ~ {
        if (is.null(.x$filter)) {
          new_data <- .data
        } else {
          new_data <- dplyr::filter(.data, !!.x$filter)
        }
        preds <- predict(.x$estimate, newdata = new_data, type = "response")
        new_data[.pred_name] <- preds
        new_data
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows)
  .data
}
