# ------------------------------- Methods --------------------------------------

cal_apply_multi <- function(object, .data, pred_class) {
  UseMethod("cal_apply_multi")
}

cal_apply_multi.cal_estimate_multinomial_multi <- function(object,
                                                           .data,
                                                           pred_class = NULL,
                                                           ...) {
  apply_multi_predict(
    object = object,
    .data = .data
  )
}

cal_apply_multi.cal_estimate_isotonic_multi <- function(object,
                                                        .data,
                                                        pred_class = NULL,
                                                        ...) {
  apply_interval_impl(
    object = object,
    .data = .data
  )
}

cal_apply_multi.cal_estimate_isotonic_boot_multi<- function(object,
                                                        .data,
                                                        pred_class = NULL,
                                                        ...) {
  apply_interval_impl(
    object = object,
    .data = .data,
    multi = TRUE
  )
}

cal_apply_multi.cal_estimate_beta_multi<- function(object,
                                                   .data,
                                                   pred_class = NULL,
                                                   ...) {
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
    tibble::as_tibble()

  for (i in seq_along(object$levels)) {
    lev <- object$levels[i]
    .data[, as.character(lev)] <- preds[, names(lev)]
  }
  .data
}

#---------------------------- >> Beta Predict ----------------------------------

apply_beta_impl <- function(object, .data) {

  # Iterates through each group
  ret <- object$estimates %>%
    purrr::map(~ {
      apply_beta_column(
        .data = .data,
        est_filter = .x$filter,
        estimates = .x$estimates
      )
    }) %>%
    purrr::reduce(dplyr::bind_rows)

  if (object$type == "binary") {
    ret[, object$levels[[2]]] <- 1 - ret[, object$levels[[1]]]
  } else {
    ols <- as.character(object$levels)
    rs <- rowSums(ret[, ols])
    for(i in seq_along(ols)) {
      ret[, ols[i]] <- ret[, ols[i]] / rs
    }
  }

  ret
}

# Iterates through each prediction column
apply_beta_column <- function(.data, est_filter, estimates) {
  if (is.null(est_filter)) {
    df <- .data
  } else {
    df <- dplyr::filter(.data, !!est_filter)
  }

  ret <- estimates %>%
    purrr::imap(~ {
      apply_beta_single(
        model = .x,
        df = df,
        est_name = .y
      )
    })

  names_ret <- names(ret)
  for(i in seq_along(names_ret)) {
    df[, names_ret[i]] <- ret[[names_ret[i]]]
  }
  df
}



apply_beta_single <- function(model, df, est_name) {
  p <- df[[est_name]]
  betacal::beta_predict(
    p = p,
    calib = model
  )
}
