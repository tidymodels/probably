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
  cal_add_cls_interval_impl(
    object = object,
    .data = .data,
    multi = TRUE
  )
}

cal_apply_binary.cal_estimate_isotonic <- function(object,
                                                    .data,
                                                    pred_class = NULL,
                                                    ...) {
  cal_add_cls_interval_impl(
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

cal_add_cls_interval_impl <- function(object, .data, multi = FALSE) {
  if (object$type == "binary") {
    proc_levels <- object$levels[1]
  } else {
    proc_levels <- object$levels
  }

  .data <- object$estimates %>%
    purrr::map(
      ~ {
        if (is.null(.x$filter)) {
          new_data <- .data
        } else {
          new_data <- dplyr::filter(.data, !!.x$filter)
        }


        curr_estimate <- .x$estimates

        intervals <- curr_estimate %>%
              purrr::imap(
                ~{
                  cal_get_cls_intervals(
                    estimates = .x,
                    .data = new_data,
                    estimate = .y
                  )
                }
              )

        if (multi) {
          intervals <- intervals %>%
            unlist() %>%
            matrix(nrow = nrow(new_data)) %>%
            apply(1, mean)
        }

        if(object$type == "binary") {
          intervals <- intervals[[1]]
          new_data[object$levels[[1]]] <- intervals[[1]]
          new_data[object$levels[[2]]] <- 1 - intervals[[1]]
        } else {
          int_df <- as.data.frame(intervals)

          cal_df <- int_df %>%
            purrr::transpose() %>%
            purrr::map(~ max_sort(as.numeric(.x))) %>%
            purrr::map(as_list) %>%
            purrr::map(purrr::set_names, colnames(int_df)) %>%
            purrr::reduce(dplyr::bind_rows)

          new_data <- new_data %>%
            dplyr::select(- !! as.character(proc_levels)) %>%
            dplyr::bind_cols(cal_df)

          # int_sums <- rowSums(int_df)
          #
          # for(i in seq_along(intervals)) {
          #   int_div <- intervals[[i]] / int_sums
          #   new_data[names(intervals[i])] <- int_div
          # }
        }
        new_data
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows)

  .data
}

max_sort <- function(x) {
  c_t <- 0
  ret <- x
  for(i in order(x, decreasing = TRUE)) {
    xi <- x[i]
    if(c_t + xi >= 1) {
      xi <- 1 - c_t
    }
    ret[i] <- xi
    c_t <- c_t + xi
  }
  ret
}


cal_get_cls_intervals <- function(estimates_table, .data, estimate) {
  y <- estimates_table$.adj_estimate
  find_interval <- findInterval(
    x = .data[[estimate]],
    vec = estimates_table$.estimate
  )
  find_interval[find_interval == 0] <- 1
  y[find_interval]
}
