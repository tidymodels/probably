# ------------------------------- Methods --------------------------------------

cal_apply_binary <- function(object, .data, pred_class) {
  UseMethod("cal_apply_binary")
}

cal_apply_binary.cal_estimate_logistic <- function(object,
                                                   .data,
                                                   pred_class = NULL,
                                                   ...) {
  cal_add_cls_predict_impl(
    object = object,
    .data = .data
  )
}

cal_apply_binary.cal_estimate_logistic_spline <- function(object,
                                                          .data,
                                                          pred_class = NULL,
                                                          ...) {
  cal_add_cls_predict_impl(
    object = object,
    .data = .data
  )
}

cal_apply_binary.cal_estimate_isotonic_boot <- function(object,
                                                        .data,
                                                        pred_class = NULL,
                                                        ...) {
  apply_interval_impl(
    object = object,
    .data = .data,
    multi = TRUE
  )
}

cal_apply_binary.cal_estimate_isotonic <- function(object,
                                                   .data,
                                                   pred_class = NULL,
                                                   ...) {
  apply_interval_impl(
    object = object,
    .data = .data
  )
}

cal_apply_binary.cal_estimate_beta <- function(object,
                                               .data,
                                               pred_class = NULL,
                                               ...) {
  if (object$type == "binary") {
    p <- dplyr::pull(.data, !!object$levels[[1]])
    model <- object$estimates[[1]]$estimate
    preds <- betacal::beta_predict(
      p = p,
      calib = model
    )
    .data[object$levels[[1]]] <- preds
    .data[object$levels[[2]]] <- 1 - preds
  }
  .data
}

#---------------------------- Adjust implementations ---------------------------

cal_add_cls_predict_impl <- function(object, .data) {
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

#---------------------------------- >> Interval --------------------------------

apply_interval_impl <- function(object, .data, multi = FALSE, method = "auto") {

  # Iterates through each group
  ret <- object$estimates %>%
    purrr::map(~ {
      apply_interval_column(
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
apply_interval_column <- function(.data, est_filter, estimates) {
  if (is.null(est_filter)) {
    df <- .data
  } else {
    df <- dplyr::filter(.data, !!est_filter)
  }

  ret <- estimates %>%
    purrr::transpose() %>%
    purrr::imap(~ {
      apply_interval_estimate(
        estimate = .x,
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

# Iterates through each model run
apply_interval_estimate <- function(estimate, df, est_name) {
  ret <- estimate %>%
    purrr::map(
      apply_interval_single,
      df = df,
      est_name = est_name
    )

  if(length(estimate) > 1) {
    ret <- ret %>%
      data.frame() %>%
      rowMeans()
  } else {
    ret <- ret[[1]]
  }

  ret
}

apply_interval_single <- function(estimates_table, df, est_name) {
  y <- estimates_table$.adj_estimate
  find_interval <- findInterval(
    x = df[[est_name]],
    vec = estimates_table$.estimate
  )
  find_interval[find_interval == 0] <- 1
  ret <- y[find_interval]
  ret
}

max_sort <- function(x) {
  c_t <- 0
  ret <- x
  if (sum(x) < 1) {
    ret <- x / sum(x)
  } else {
    for (i in order(x, decreasing = TRUE)) {
      xi <- x[i]
      if (c_t + xi >= 1) {
        xi <- 1 - c_t
      }
      ret[i] <- xi
      c_t <- c_t + xi
    }
  }
  ret
}
