# ------------------------------- Methods --------------------------------------

cal_apply_multi <- function(object, .data, pred_class) {
  UseMethod("cal_apply_multi")
}

cal_apply_multi.cal_estimate_multinomial <-
  function(object, .data, pred_class = NULL, ...) {

    apply_multi_predict(
      object = object,
      .data = .data
    )
  }

cal_apply_multi.cal_estimate_beta <-
  function(object, .data, pred_class = NULL, ...) {
    apply_beta_impl(
      object = object,
      .data = .data
    )
  }

#---------------------------- Adjust implementations ---------------------------

#---------------------------- >> Single Predict --------------------------------

apply_multi_predict <- function(object, .data) {
  preds <- object$estimates[[1]]$estimate %>%
    predict(newdata = .data, type = "probs") %>%
    dplyr::as_tibble()

  for (i in seq_along(object$levels)) {
    lev <- object$levels[i]
    .data[, as.character(lev)] <- preds[, names(lev)]
  }
  .data
}
