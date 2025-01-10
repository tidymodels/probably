# ------------------------------- Methods --------------------------------------

cal_apply_regression <- function(object, .data, pred_class) {
  UseMethod("cal_apply_regression")
}

#' @export
cal_apply_regression.cal_estimate_linear_spline <- function(
  object,
  .data,
  pred_class = NULL,
  ...
) {
  apply_reg_predict(
    object = object,
    .data = .data
  )
}

#' @export
cal_apply_regression.cal_estimate_linear <- cal_apply_regression.cal_estimate_linear_spline

#---------------------------- Adjust implementations ---------------------------

apply_reg_predict <- function(object, .data) {
  .pred_name <- rlang::expr_deparse(object$levels$predictions)
  resid <- inherits(object, "residual_adjustment")
  .data <- object$estimates %>%
    purrr::map(make_predictions, .data = .data, nm = .pred_name, resid = resid) %>%
    purrr::reduce(dplyr::bind_rows)
  .data
}

make_predictions <- function(x, .data, nm, resid) {
  if (is.null(x$filter)) {
    new_data <- .data
  } else {
    new_data <- dplyr::filter(.data, !!x$filter)
  }
  preds <- predict(x$estimate, newdata = new_data, type = "response")
  if (resid) {
    new_data[nm] <- new_data[nm] + preds
  } else {
    new_data[nm] <- preds
  }

  new_data
}
