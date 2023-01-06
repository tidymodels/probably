# ------------------------------- Methods --------------------------------------

cal_adjust_binary <- function(object, .data, pred_class) {
  UseMethod("cal_adjust_binary")
}

cal_adjust_binary.cal_estimate_logistic <- function(object,
                                                    .data,
                                                    pred_class = NULL,
                                                    ...) {
  cal_add_predict_impl(
    object = object,
    .data = .data
  )
}

cal_adjust_binary.cal_estimate_logistic_spline <- function(object,
                                                           .data,
                                                           pred_class = NULL,
                                                           ...) {
  cal_add_predict_impl(
    object = object,
    .data = .data
  )
}

cal_adjust_binary.cal_estimate_isotonic_boot <- function(object,
                                                         .data,
                                                         pred_class = NULL,
                                                         ...) {
  cal_add_interval_impl(
    object = object,
    .data = .data,
    multi = TRUE
  )
}

cal_adjust_binary.cal_estimate_isotonic <- function(object,
                                                    .data,
                                                    pred_class = NULL,
                                                    ...) {
  cal_add_interval_impl(
    object = object,
    .data = .data
  )
}

cal_adjust_binary.cal_estimate_beta <- function(object,
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

cal_add_predict_impl <- function(object, .data) {
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

cal_add_interval_impl <- function(object, .data, multi = FALSE) {
  if (object$type == "binary") {
    level_1 <- object$levels[[1]]
    level_2 <- object$levels[[2]]

    .data <- object$estimates %>%
      purrr::map(
        ~ {
          if (is.null(.x$filter)) {
            new_data <- .data
          } else {
            new_data <- dplyr::filter(.data, !!.x$filter)
          }

          if (!multi) {
            intervals <- cal_get_intervals(
              estimates_table = .x$estimates[[1]],
              .data = new_data,
              estimate = level_1
            )
          } else {
            intervals <- .x$estimates %>%
              map(cal_get_intervals,
                  .data = new_data,
                  estimate = level_1
              ) %>%
              unlist() %>%
              matrix(nrow = nrow(new_data)) %>%
              apply(1, mean)
          }
          new_data[level_1] <- intervals
          new_data[level_2] <- 1 - intervals
          new_data
        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)
  }
  .data
}

cal_get_intervals <- function(estimates_table, .data, estimate) {
  y <- estimates_table$.adj_estimate
  find_interval <- findInterval(
    x = .data[[estimate]],
    vec = estimates_table$.estimate
  )
  find_interval[find_interval == 0] <- 1
  y[find_interval]
}

cal_update_prediction <- function(.data, object, pred_class) {
  res <- .data
  if (!is.null(pred_class)) {

    pred_name <- as_name(pred_class)
    if (pred_name %in% colnames(.data)) {
      .data[, pred_name] <- NULL
    }

    if (object$type == "binary") {
      level1_gt <- res[[object$levels[[1]]]] > res[[object$levels[[2]]]]
      res[level1_gt, pred_name] <- names(object$levels[1])
      res[!level1_gt, pred_name] <- names(object$levels[2])
      res[, pred_name] <- as.factor(res[, pred_name][[1]])
    }

    if (object$type == "multiclass") {
      max_cols <- max.col(res[, as.character(object$levels)])
      factor_cols <- as.factor(max_cols)
      levels(factor_cols) <-names(object$levels)
      res[, pred_name] <- factor_cols
    }
  }
  res
}
