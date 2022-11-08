cal_binary_plot_breaks <- function(.data,
                                   truth,
                                   estimate,
                                   num_breaks = 10,
                                   conf_level = 0.90,
                                   include_rug = TRUE,
                                   include_ribbon = TRUE
                                   ) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_name <- as_name(truth)

  truth_levels <- levels(.data[truth_name][[1]])

  if(length(truth_levels) != 2) stop("'", truth_name, "' does not have 2 levels")

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  prob_tbl <- probability_breaks(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    num_breaks = num_breaks,
    conf_level = conf_level
  )

  sub_title <- paste0("'", truth_name, "' is equal to '", truth_levels[[1]], "'")

  res <- ggplot(data = prob_tbl, aes(x = predicted_midpoint)) +
    geom_abline(col = "green", lty = 2) +
    geom_line(aes(y = event_rate)) +
    geom_point(aes(y = event_rate))

  if(include_ribbon) {
    res <- res +
      geom_ribbon(aes(y = event_rate, ymin = lower, ymax = upper), alpha = 1 / 5)
  }

  if(include_rug) {
    level_1_tbl <- dplyr::filter(.data, as.integer(!!truth) == 1)
    level_2_tbl <- dplyr::filter(.data, as.integer(!!truth) != 1)
    res <- res +
      geom_rug(
        data = level_1_tbl,
        aes(x = !!estimate, col = !!truth),
        sides = "b",
        cex = 1 / 5,
        show.legend = FALSE
        ) +
      geom_rug(
        data = level_2_tbl,
        aes(x = !!estimate, col = !!truth),
        sides = "t",
        cex = 1 / 5,
        show.legend = FALSE
        )
  }

  res +
    coord_obs_pred() +
    labs(
      title = "Calibration Plot",
      subtitle = sub_title,
      x = "Predicted Midpoint",
      y = "Event Rate"
    )
}

cal_binary_plot_logistic <- function(.data,
                                     truth,
                                     estimate,
                                     conf_level = 0.90,
                                     include_rug = TRUE,
                                     include_ribbon = TRUE
                                     ) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_name <- as_name(truth)

  truth_levels <- levels(.data[truth_name][[1]])

  if(length(truth_levels) != 2) stop("'", truth_name, "' does not have 2 levels")

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  prob_tbl <- probability_gam(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate
  )

  sub_title <- paste0("'", truth_name, "' is equal to '", truth_levels[[1]], "'")

  res <- ggplot(data = prob_tbl, aes(x = estimate)) +
    geom_abline(col = "green", lty = 2) +
    geom_line(aes(y = prob))

  if(include_ribbon) {
    res <- res +
      geom_ribbon(aes(y = prob, ymin = lower, ymax = upper), alpha = 1 / 5)
  }

  if(include_rug) {
    level_1_tbl <- dplyr::filter(.data, as.integer(!!truth) == 1)
    level_2_tbl <- dplyr::filter(.data, as.integer(!!truth) != 1)
    res <- res +
      geom_rug(
        data = level_1_tbl,
        aes(x = !!estimate, col = !!truth),
        sides = "b",
        cex = 1 / 5,
        show.legend = FALSE
      ) +
      geom_rug(
        data = level_2_tbl,
        aes(x = !!estimate, col = !!truth),
        sides = "t",
        cex = 1 / 5,
        show.legend = FALSE
      )
  }

  res +
    coord_obs_pred() +
    labs(
      title = "Calibration Plot",
      subtitle = sub_title,
      x = "Predicted Midpoint",
      y = "Event Rate"
    )
}

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
    dplyr::ungroup() %>%
    dplyr::select(-.bin)

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
        ret$lower <- pt$conf.int[[1]]
        ret$upper <- pt$conf.int[[2]]
        ret
      }
    )
}

probability_gam <- function(.data, truth, estimate) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  prep_data <- dplyr::select(.data, truth = !!truth, estimate = !!estimate)

  model <- mgcv::gam(
    truth ~ s(estimate, k = 10),
    data = prep_data,
    family = binomial()
    )
  new_seq <- seq(0, 1, by = .01)
  new_data <- data.frame(estimate = new_seq)
  preds <- predict(model, new_data, type = "response", se.fit = TRUE)

  res <- tibble(
    prob = 1 - preds$fit,
    se_fit = preds$se.fit
  )

  res <- cbind(new_data, res)

  res$lower <- res$prob - qnorm(0.95) * res$se_fit
  res$upper <- res$prob + qnorm(0.95) * res$se_fit
  res$se_fit <- NULL

  as_tibble(res)

}
