#---------------------------------- >> Interval --------------------------------
apply_interval_impl <- function(object, .data, multi = FALSE, method = "auto") {
  # Iterates through each group
  new_data <- object$estimates %>%
    purrr::map(~ {
      apply_interval_column(
        .data = .data,
        est_filter = .x$filter,
        estimates = .x$estimates
      )
    }) %>%
    purrr::reduce(dplyr::bind_rows)

  apply_adjustment(new_data, object)

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
  for (i in seq_along(names_ret)) {
    df[, names_ret[i]] <- ret[[names_ret[i]]]
  }
  df
}

# Iterates through each model run
apply_interval_estimate <- function(estimate, df, est_name) {
  # Handles single quoted variable names, which are typically created
  # when there are spaces in the original variable name
  df_names <- names(df)
  if (!(est_name %in% df_names)) {
    test_name <- sub("`", "", est_name)
    test_name <- sub("`", "", test_name)
    if (test_name %in% df_names) {
      est_name <- test_name
    } else {
      rlang::abort(paste0("Variable: ", est_name, " was not found in data"))
    }
  }

  ret <- estimate %>%
    purrr::map(
      apply_interval_single,
      df = df,
      est_name = est_name
    )

  if (length(estimate) > 1) {
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


#---------------------------- >> Beta Predict ----------------------------------

apply_beta_impl <- function(object, .data) {
  # Iterates through each group
  new_data <- object$estimates %>%
    purrr::map(~ {
      apply_beta_column(
        .data = .data,
        est_filter = .x$filter,
        estimates = .x$estimate
      )
    }) %>%
    purrr::reduce(dplyr::bind_rows)

  apply_adjustment(new_data, object)
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
  for (i in seq_along(names_ret)) {
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

# ------------------------------  Adjustment -----------------------------------

apply_adjustment <- function(new_data, object) {
  if (object$type == "binary") {
    new_data[, object$levels[[2]]] <- 1 - new_data[, object$levels[[1]]]
  }

  if(object$type == "one_vs_all"){
    ols <- as.character(object$levels)
    rs <- rowSums(new_data[, ols])
    for (i in seq_along(ols)) {
      new_data[, ols[i]] <- new_data[, ols[i]] / rs
    }
  }

  new_data
}
