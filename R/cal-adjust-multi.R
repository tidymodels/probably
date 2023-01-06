# ------------------------------- Methods --------------------------------------

cal_adjust_multi <- function(object, .data, pred_class) {
  UseMethod("cal_adjust_multi")
}

cal_adjust_multi.cal_estimate_multinomial <- function(object,
                                                      .data,
                                                      pred_class = NULL,
                                                      ...) {
  cal_multi_predict(
    object = object,
    .data = .data
  )
}

#---------------------------- Adjust implementations ---------------------------

cal_multi_predict <- function(object, .data) {
  if (object$type == "multiclass") {
    preds <- object$estimates[[1]]$estimate %>%
      predict(newdata = .data, type = "probs") %>%
      tibble::as_tibble()


    for (i in seq_along(object$levels)) {
      lev <- object$levels[i]
      .data[, as.character(lev)] <- preds[, names(lev)]
    }
    .data
  }
}
