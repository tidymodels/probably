#--------------------------------- Table ---------------------------------------

# This function iterates through each of the class levels. For binary it selects
# the appropiate one based on the `event_level` selected
.cal_class_grps <- function(.data, truth, cuts, levels, event_level, conf_level) {
  truth <- enquo(truth)

  lev <- process_level(event_level)
  length_levels <- length(levels)

  if (length_levels == 2) {
    levels <- levels[[1]]
    if(lev == 0) {
      lev <- 1
    }
  }

  if (length_levels > 2 & lev != 0) {
    msg <- "Only 'event_level' of 'auto' is valid for multi-class models"
    rlang::abort(msg)
  }

  no_levels <- levels

  names(no_levels) <- seq_along(no_levels)

  res <- purrr::imap(
    no_levels,
    ~ {
      .cal_cut_grps(
        .data = .data,
        truth = !!truth,
        estimate = !!.x,
        cuts = cuts,
        level = as.integer(.y),
        lev = lev,
        conf_level = conf_level
      )
    }
  )

  if (length(res) > 1) {
    res <- res %>%
      purrr::set_names(names(levels)) %>%
      purrr::imap(~ dplyr::mutate(.x, !!truth := .y)) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::select(!!truth, dplyr::everything())
  } else {
    res <- res[[1]]
  }

  res
}

# This function iterates through each breaks/windows of the plot
.cal_cut_grps <- function(.data, truth, estimate, cuts,
                          level, lev, conf_level
                          ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)


  cuts %>%
    purrr::transpose() %>%
    purrr::map_df(
      ~ {
        .data %>%
          dplyr::filter(
            !!estimate >= !!.x$lower_cut & !!estimate <= !!.x$upper_cut
          ) %>%
          process_midpoint(
            truth = !!truth,
            estimate = !!estimate,
            level = level,
            lev = lev,
            conf_level = conf_level
          ) %>%
          dplyr::mutate(
            predicted_midpoint = .x$lower_cut + ((.x$upper_cut - .x$lower_cut) / 2)
          ) %>%
          dplyr::select(predicted_midpoint, dplyr::everything())
      }
    )
}

#------------------------------- >> Utils --------------------------------------
process_midpoint <- function(.data, truth, estimate, group = NULL, .bin = NULL,
                             level = 1, lev = 1, conf_level = 0.95) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)
  .bin <- enquo(.bin)

  if(lev == 2) {
    lev_yes <- 0
    lev_no <- 1
  } else {
    lev_yes <- 1
    lev_no <- 0
  }

  tbl <- .data %>%
    dplyr::mutate(
      .bin = !!.bin,
      .is_val = ifelse(as.integer(!!truth) == level, lev_yes, lev_no)
    )

  if (!quo_is_null(group)) tbl <- dplyr::group_by(tbl, !!group, .add = TRUE)
  if (!quo_is_null(.bin)) tbl <- dplyr::group_by(tbl, !!.bin, .add = TRUE)

  tbl <- tbl %>%
    dplyr::summarise(
      event_rate = sum(.is_val, na.rm = TRUE) / dplyr::n(),
      events = sum(.is_val, na.rm = TRUE),
      total = dplyr::n()
    ) %>%
    dplyr::filter(total > 0)

  if (!quo_is_null(.bin)) tbl <- dplyr::select(tbl, -.bin)

  add_conf_intervals(
    .data = tbl,
    events = events,
    total = total,
    conf_level = conf_level
  )
}

add_conf_intervals <- function(.data,
                               events = events,
                               total = total,
                               conf_level = 0.90) {
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
        ret <- dplyr::as_tibble(.x)
        ret$lower <- pt$conf.int[[1]]
        ret$upper <- pt$conf.int[[2]]
        ret
      }
    )
}

process_level <- function(x) {
  x <- x[[1]]
  ret <- NULL
  if (x == "auto") {
    ret <- 0
  }
  if (x == "first") {
    ret <- 1
  }
  if (x == "second") {
    ret <- 2
  }
  if (is.null(ret)) {
    msg <- "Invalid event_level entry. Valid entries are 'first', 'second', or 'auto'"
    rlang::abort(msg)
  }
  ret
}

assert_truth_two_levels <- function(.data, truth) {
  truth <- enquo(truth)
  if (!quo_is_null(truth)) {
    truth_name <- as_name(truth)
    truth_levels <- levels(.data[truth_name][[1]])
    if (length(truth_levels) != 2) {
      rlang::abort(paste0("'", truth_name, "' should be a factor with 2 levels"))
    }
  }
}

tune_results_args <- function(.data,
                              truth,
                              estimate,
                              group,
                              event_level,
                              parameters = NULL,
                              ...) {
  if (!(".predictions" %in% colnames(.data))) {
    rlang::abort(
      paste0(
        "The `tune_results` object does not contain columns with predictions",
        " Refit with the control argument `save_pred = TRUE` to save these columns."
      )
    )
  }

  predictions <- tune::collect_predictions(
    x = .data,
    summarize = TRUE,
    parameters = parameters,
    ...
  )

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  if (quo_is_null(truth)) {
    truth_str <- attributes(.data)$outcome
    truth <- parse_expr(truth_str)
  }

  if (quo_is_null(estimate)) {
    truth_str <- as_name(truth)
    lev <- process_level(event_level) # TODO changes for regression?
    fc_truth <- levels(predictions[[truth_str]])
    estimate_str <- paste0(".pred_", fc_truth[[lev]])
    estimate <- parse_expr(estimate_str)
  }

  if (quo_is_null(group)) {
    group <- quo(.config)
  }

  list(
    truth = quo(!!truth),
    estimate = quo(!!estimate),
    group = quo(!!group),
    predictions = predictions
  )
}

#------------------------------- >> Plot ---------------------------------------

binary_plot_impl <- function(tbl, x, y,
                             .data, truth, estimate, group,
                             x_label, y_label,
                             include_ribbon, include_rug, include_points,
                             is_tune_results = FALSE) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  group <- enquo(group)

  x <- enquo(x)
  y <- enquo(y)

  tbl_groups <- dplyr::group_vars(tbl)

  gp_vars <- dplyr::group_vars(.data)

  if (length(gp_vars)) {
    if (length(gp_vars) > 1) {
      rlang::abort("Plot does not support more than one grouping variable")
    }
    has_groups <- TRUE
    dplyr_group <- parse_expr(gp_vars)
    grouping_var <- tbl[, gp_vars][[1]]
    if (is.numeric(grouping_var)) {
      tbl[, gp_vars] <- as.factor(format(grouping_var))
    }
  } else {
    has_groups <- FALSE
    dplyr_group <- NULL
  }

  res <- ggplot(data = tbl, aes(x = !!x, color = !!dplyr_group, fill = !!dplyr_group)) +
    geom_abline(col = "#aaaaaa", linetype = 2) +
    geom_line(aes(y = !!y))

  if (include_points) {
    res <- res + geom_point(aes(y = !!y))
  }

  if (include_ribbon) {
    res <- res +
      geom_ribbon(
        aes(y = !!y, ymin = lower, ymax = upper),
        color = "#ffffff00",
        alpha = 0.08
      )
  }

  if (include_rug & !has_groups & !length(tbl_groups) & !is_tune_results) {
    levels <- truth_estimate_map(
      .data = .data,
      truth = !!truth,
      estimate = !!estimate
    )

    level1 <- levels[[1]]

    if (length(levels) > 1) {
      rlang::warn(paste0("Multiple class columns identified. Using: `", level1, "`"))
    }

    truth_values <- 1:2
    side_values <- c("t", "b")
    for (i in seq_along(truth_values)) {
      level_tbl <- dplyr::filter(.data, as.integer(!!truth) == truth_values[i])
      res <- res +
        geom_rug(
          data = level_tbl,
          aes(x = !!level1),
          color = "#999999",
          sides = side_values[i],
          length = unit(0.015, "npc"),
          alpha = 0.7,
          show.legend = FALSE
        )
    }
  }

  res <- res +
    lims(x = 0:1, y = 0:1) +
    labs(
      x = x_label,
      y = y_label
    ) +
    theme_light() +
    theme(aspect.ratio = 1)

  if (!quo_is_null(group) & length(tbl_groups)) {
    res <- res + facet_grid(
      rows = vars(!!group),
      cols = vars(!!parse_expr(tbl_groups))
    )
  } else {
    if (!quo_is_null(group)) {
      res <- res + facet_wrap(group)
    }
    if (length(tbl_groups)) {
      res <- res + facet_wrap(tbl_groups)
    }
  }

  res
}
