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
