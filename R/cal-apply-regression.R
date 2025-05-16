# ------------------------------- Methods --------------------------------------

cal_apply_regression <- function(object, .data, pred_class) {
  UseMethod("cal_apply_regression")
}

#' @export
cal_apply_regression.cal_estimate_linear_spline <-
  function(object, .data, pred_class = NULL, ...) {
    apply_reg_predict(
      object = object,
      .data = .data
    )
  }

#' @export
cal_apply_regression.cal_estimate_linear <-
  cal_apply_regression.cal_estimate_linear_spline

#---------------------------- Adjust implementations ---------------------------

numeric_repredict <- function(x, predict_data, prd_nm) {
  if (is.null(x$filter)) {
    new_data <- predict_data
  } else {
    new_data <- dplyr::filter(predict_data, !!x$filter)
  }
  preds <- predict(x$estimate, newdata = new_data, type = "response")
  new_data[prd_nm] <- preds
  new_data
}

apply_reg_predict <- function(object, .data) {
  .data <-
    purrr::map(
      object$estimates,
      numeric_repredict,
      predict_data = .data,
      prd_nm = rlang::expr_deparse(object$levels$predictions)
    ) |>
    purrr::reduce(dplyr::bind_rows)
  .data
}
