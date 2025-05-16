#--------------------------------- Table ---------------------------------------

# This function iterates through each of the class levels. For binary it selects
# the appropriate one based on the `event_level` selected
.cal_class_grps <- function(.data, truth, cuts, levels, event_level, conf_level,
                            method = "breaks", smooth = NULL) {
  truth <- enquo(truth)

  lev <- process_level(event_level)
  length_levels <- length(levels)

  if (length_levels >= 2 & lev == 0) {
    lev <- 1
  }

  if (length_levels == 2) {
    levels <- levels[1]
  }

  if (length_levels > 2 & lev == 2) {
    cli::cli_abort("Only {.val auto} {.arg event_level} is valid for multi-class models.")
  }

  no_levels <- levels

  names(no_levels) <- seq_along(no_levels)

  if (method == "breaks") {
    res <- purrr::imap(
      no_levels,
      ~ .cal_cut_grps(
        .data = .data,
        truth = !!truth,
        estimate = !!.x,
        cuts = cuts,
        level = as.integer(.y),
        lev = lev,
        conf_level = conf_level
      )
    )
  }

  if (method == "model") {
    res <- purrr::imap(
      no_levels,
      ~ .cal_model_grps(
        .data = .data,
        truth = !!truth,
        estimate = !!.x,
        smooth = smooth,
        level = as.integer(.y),
        lev = lev,
        conf_level = conf_level
      )
    )
  }

  if (length(res) > 1) {
    res <- res |>
      purrr::set_names(names(levels)) |>
      purrr::imap(~ dplyr::mutate(.x, !!truth := .y)) |>
      purrr::reduce(dplyr::bind_rows) |>
      dplyr::select(!!truth, dplyr::everything())
  } else {
    res <- res[[1]]
  }

  if (method == "breaks") {
    res <- res |>
      dplyr::select(predicted_midpoint, dplyr::everything())
  }

  res
}

.cal_model_grps <- function(.data,
                            truth = NULL,
                            estimate = NULL,
                            conf_level = 0.90,
                            event_level = c("auto", "first", "second"),
                            lev,
                            level,
                            smooth = TRUE) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if (lev == 2) {
    lev_yes <- 0
    lev_no <- 1
  } else {
    lev_yes <- 1
    lev_no <- 0
  }

  prep_data <- .data |>
    dplyr::select(truth = !!truth, estimate = !!estimate) |>
    dplyr::mutate(
      truth = ifelse(as.integer(truth) == level, lev_yes, lev_no)
    )

  if (smooth) {
    model <- mgcv::gam(
      truth ~ s(estimate, k = 10),
      data = prep_data,
      family = binomial()
    )
  } else {
    model <- stats::glm(
      truth ~ estimate,
      data = prep_data,
      family = binomial()
    )
  }

  new_seq <- seq(0, 1, by = .01)
  new_data <- data.frame(estimate = new_seq)
  preds <- predict(model, new_data, se.fit = TRUE)

  res <- data.frame(
    prob = binomial()$linkinv(preds$fit),
    lower = binomial()$linkinv(preds$fit - qnorm(conf_level) * preds$se.fit),
    upper = binomial()$linkinv(preds$fit + qnorm(conf_level) * preds$se.fit)
  )

  res <- cbind(new_data, res)

  dplyr::as_tibble(res)
}

# This function iterates through each breaks/windows of the plot
.cal_cut_grps <- function(.data, truth, estimate, cuts,
                          level, lev, conf_level) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if (lev == 2) {
    lev_yes <- 0
    lev_no <- 1
  } else {
    lev_yes <- 1
    lev_no <- 0
  }

  .data <- .data |>
    dplyr::mutate(
      .is_val = ifelse(as.integer(!!truth) == level, lev_yes, lev_no),
      .estimate = !!estimate
    )

  cuts |>
    purrr::list_transpose(simplify = FALSE) |>
    purrr::map_df(mid_point, pred_data = .data, conf_level = conf_level)
}


mid_point <- function(x, pred_data, conf_level) {
  rf <- pred_data$.estimate >= x$lower_cut & pred_data$.estimate <= x$upper_cut
  ret <- pred_data[rf, ]
  ret <- process_midpoint(ret, conf_level = conf_level)
  if (!is.null(ret)) {
    pm <- x$lower_cut + ((x$upper_cut - x$lower_cut) / 2)
    ret$predicted_midpoint <- pm
  }
  ret
}

process_midpoint <- function(.data, conf_level = 0.95) {
  events <- sum(.data$.is_val, na.rm = TRUE)
  total <- nrow(.data)

  tbl <- NULL

  if (total > 0) {
    suppressWarnings(
      pt <- prop.test(events, total, conf.level = conf_level)
    )

    tbl <- data.frame(
      event_rate = events / total,
      events = events,
      total = total,
      lower = pt$conf.int[[1]],
      upper = pt$conf.int[[2]]
    )
  }

  tbl
}

#---------------------------------- Utils --------------------------------------

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
    cli::cli_abort("Invalid {.arg event_level} entry: {x}. Valid entries are
                   {.val first}, {.val second}, or {.val auto}.", call = NULL)
  }
  ret
}

tune_results_args <- function(.data,
                              truth,
                              estimate,
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

  if (quo_is_null(truth)) {
    truth_str <- attributes(.data)$outcome
    truth <- parse_expr(truth_str)
  }

  if (quo_is_null(estimate)) {
    estimate <- expr(dplyr::starts_with(".pred"))
  }

  if (dplyr::n_distinct(.data[[".predictions"]][[1]][[".config"]]) > 1) {
    group <- quo(.config)
  } else {
    group <- quo(NULL)
  }

  list(
    truth = quo(!!truth),
    estimate = quo(!!estimate),
    estimate = estimate,
    group = group,
    predictions = predictions
  )
}

#--------------------------------- Plot ----------------------------------------

cal_plot_impl <- function(tbl, x, y,
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
      cli::cli_abort("Plot does not support more than one grouping variable.")
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

    if (length(levels) > 1 & !is_tune_results) {
      cli::cli_warn("Multiple class columns identified. Using: {.code {level1}}")
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

quo_to_sym <- function(x, .data) {
  res <- tidyselect::eval_select(x, .data)
  if (length(res) == 0) {
    return(NULL)
  }
  res <- names(res)
  rlang::sym(res)
}
