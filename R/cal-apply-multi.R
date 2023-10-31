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

#---------------------------- Adjust implementations ---------------------------

#---------------------------- >> Single Predict --------------------------------

apply_multi_predict <- function(object, .data) {
  if (inherits(object$estimates[[1]]$estimate, "gam")) {
    prob_type <- "response"
  } else {
    prob_type <- "probs"
  }
  preds <- object$estimates[[1]]$estimate %>%
    predict(newdata = .data, type = prob_type)

  colnames(preds) <- as.character(object$levels)
  preds <- dplyr::as_tibble(preds)

  for (i in seq_along(object$levels)) {
    lev <- object$levels[i]
    .data[, as.character(lev)] <- preds[, as.character(lev)]
  }
  .data
}
