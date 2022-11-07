probability_breaks <- function(.data,
                               truth,
                               estimate,
                               num_breaks = 10,
                               conf_level = 0.90
                               ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  bin_exprs <- map(
    seq_len(num_breaks),
    ~ expr(!!estimate <= !!.x / !!num_breaks ~ !!.x)
  )

  bin_data <- .data %>%
    dplyr::mutate(
      .bin = case_when(!!!bin_exprs),
      .is_val = ifelse(as.integer(!!truth) == 1, 1, 0)
    ) %>%
    dplyr::group_by(.bin, .add = TRUE) %>%
    dplyr::summarise(
      predicted_midpoint = median(!!estimate),
      event_rate = sum(.is_val) / n(),
      events = sum(.is_val),
      total = n()
    ) %>%
    dplyr::ungroup()

  add_conf_intervals(bin_data, events, total, conf_level = conf_level)
}

add_conf_intervals <- function(.data,
                               events = events,
                               total = total,
                               conf_level = 0.90
                               ) {
  events <- enquo(events)
  total <- enquo(total)
  .data %>%
    purrr::transpose() %>%
    purrr::map_df(
      ~ {
        events <- .x[[as_name(events)]]
        total <- .x[[as_name(total)]]
        suppressWarnings(
          pt <- prop.test(events, total, conf.level = conf_level)
        )
        ret <- as_tibble(.x)
        ret$conf_low <- pt$conf.int[[1]]
        ret$conf_high <- pt$conf.int[[2]]
        ret
      }
    )
}
