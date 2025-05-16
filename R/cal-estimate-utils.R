#-------------------------- Print methods -------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  print_cls_cal(x, ...)
}

#' @export
print.cal_estimate_isotonic <- function(x, ...) {
  print_cls_cal(x, upv = TRUE, ...)
}

#' @export
print.cal_multi <- function(x, ...) {
  print_cls_cal(x, ...)
}

#' @export
print.cal_regression <- function(x, ...) {
  print_reg_cal(x, ...)
}

print_cls_cal <- function(x, upv = FALSE, ...) {
  print_type <-
    switch(x$type,
      "binary" = "Binary",
      "multiclass" = "Multiclass",
      "one_vs_all" = "Multiclass (1 v All)",
      "regression" = "Regression",
      NA_character_
    )

  cli::cli_div(theme = list(
    span.val0 = list(color = "blue"),
    span.val1 = list(color = "yellow"),
    span.val2 = list(color = "darkgreen")
  ))
  rows <- prettyNum(x$rows, ",")
  cli::cli_h3("Probability Calibration")
  cli::cli_text("Method: {.val2 {x$method}}")
  cli::cli_text("Type: {.val2 {print_type}}")
  cli::cli_text("Source class: {.val2 {x$source_class}}")
  if (length(x$estimates) == 1) {
    cli::cli_text("Data points: {.val2 {rows}}")
  } else {
    no_ests <- length(x$estimates)
    grps <- "Data points: {.val2 {rows}}, split in {.val2 {no_ests}} groups"
    cli::cli_text(grps)
  }

  if (upv && x$type %in% c("binary", "regression")) {
    upv_no <- prettyNum(nrow(x$estimates[[1]]$estimate[[1]][[1]]), ",")
    cli::cli_text("Unique Predicted Values: {.val2 {upv_no}}")
  }

  cli::cli_text("Truth variable: `{.val0 {x$truth}}`")
  cli::cli_text("Estimate variables:")

  for (i in seq_along(x$levels)) {
    cli::cli_text("{.val1 `{x$levels[[i]]}`} ==> {.val0 {names(x$levels[i])}}")
  }

  cli::cli_end()
}


print_reg_cal <- function(x, upv = FALSE, ...) {
  cli::cli_div(theme = list(
    span.val0 = list(color = "blue"),
    span.val1 = list(color = "yellow"),
    span.val2 = list(color = "darkgreen")
  ))
  rows <- prettyNum(x$rows, ",")
  cli::cli_h3("Regression Calibration")
  cli::cli_text("Method: {.val2 {x$method}}")
  cli::cli_text("Source class: {.val2 {x$source_class}}")
  if (length(x$estimates) == 1) {
    cli::cli_text("Data points: {.val2 {rows}}")
  } else {
    no_ests <- length(x$estimates)
    grps <- "Data points: {.val2 {rows}}, split in {.val2 {no_ests}} groups"
    cli::cli_text(grps)
  }

  if (upv) {
    upv_no <- prettyNum(nrow(x$estimates[[1]]$estimate[[1]]), ",")
    cli::cli_text("Unique Predicted Values: {.val2 {upv_no}}")
  }
  x$truth <- rlang::sym(x$truth)
  cli::cli_text("Truth variable: `{.val0 {x$truth}}`")
  cli::cli_text("Estimate variable: {.val1 `{x$levels[[1]]}`}")

  cli::cli_end()
}

# ------------------------ Estimate name methods -------------------------------

cal_class_name <- function(x) {
  UseMethod("cal_class_name")
}

#' @export
cal_class_name.data.frame <- function(x) {
  "Data Frame"
}

#' @export
cal_class_name.tune_results <- function(x) {
  "Tune Results"
}

#' @export
cal_class_name.tune_results <- function(x) {
  "Tune Results"
}

#' @export
cal_class_name.rset <- function(x) {
  "Resampled data set"
}

# ------------------------------- Data Ingestion -------------------------------


get_tune_data <- function(x, parameters = NULL) {
  .data <- collect_predictions(
    x,
    summarize = TRUE,
    parameters = parameters
  )

  num_configs <- vctrs::vec_unique_count(.data$.config)
  if (num_configs > 1) {
    group <- ".config"
  } else {
    group <- character(0)
  }

  truth <- tune::.get_tune_outcome_names(x)
  lvls <- levels(.data[[truth]])

  if (is.null(lvls)) {
    estimate <- ".pred"
    lvl_map <- rlang::syms(estimate)
    names(lvl_map) <- "predictions"
  } else {
    estimate <- paste0(".pred_", lvls)
    lvl_map <- rlang::syms(estimate)
    names(lvl_map) <- lvls
  }

  list(
    truth = truth,
    estimate = estimate,
    group = group,
    predictions = .data[, c(truth, estimate, group)],
    levels = lvls,
    map = lvl_map,
    source = "tune_results"
  )
}

get_prediction_data <- function(
    .data,
    truth = NULL,
    estimate = dplyr::starts_with(".pred_"),
    .by = NULL
) {
  if (!inherits(.data, "tbl_df")) {
    data <- dplyr::as_tibble(.data)
  }

  truth <- names(tidyselect::eval_select(rlang::enquo(truth), .data))
  estimate <- names(tidyselect::eval_select(rlang::enquo(estimate), .data))
  by_vars <- names(tidyselect::eval_select(rlang::enquo(.by), .data))

  # So that we ignore non-numeric columns that are accidentally selected such
  # as `.pred_class`
  is_num_est <- purrr::map_lgl(.data[,estimate], is.numeric)
  estimate <- estimate[is_num_est]

  lvls <- levels(.data[[truth]])

  if (!is.null(lvls)) {
    if (length(lvls) != length(estimate)) {
      cli::cli_abort(
        "The selectors in {.arg estimate} resolves to {length(estimate)} values
        ({.val {estimate}}) but there are {length(lvls)} class levels
        ({.val {lvls}})."
      )
    }

    estimate <- check_tm_format(estimate, lvls)

    lvl_map <- rlang::syms(estimate)
    names(lvl_map) <- lvls
  } else {
    lvl_map <- rlang::syms(estimate)
    names(lvl_map) <- "predictions"
  }

  nms <- names(.data)
  if (any(nms == ".config")) {
    num_configs <- vctrs::vec_unique_count(.data$.config)
    if (num_configs > 1) {
      group <- ".config"
    } else {
      group <- character(0)
    }
  } else {
    group <- character(0)
  }
  group <- c(group, by_vars)

  # TODO make 2+ groupings work for filter and checks in get_group_argument

  list(
    truth = truth,
    estimate = estimate,
    group = group,
    predictions = .data[, c(truth, estimate, group)],
    levels = lvls,
    map = lvl_map,
    source = "data"
  )
}

check_tm_format <- function(estimate, lvls) {
  # Check to see if the probability columns use the tidymodels convention and
  # then make sure that they are ordered correctly.

  tm_nms <- paste0(".pred_", lvls)
  if (identical(sort(estimate), sort(tm_nms))) {

    estimate <- tm_nms
  }
  estimate
}

# ------------------------------- Utils ----------------------------------------

as_regression_cal_object <- function(estimate,
                                     truth,
                                     levels,
                                     method,
                                     rows,
                                     additional_class = NULL,
                                     source_class = NULL) {

  as_cal_object(
    estimate = estimate,
    truth = truth,
    levels = levels,
    method = method,
    rows = rows,
    additional_classes = additional_class,
    source_class = source_class,
    type = "regression"
  )
}

as_cal_object <- function(estimate,
                          truth,
                          levels,
                          method,
                          rows,
                          additional_classes = NULL,
                          source_class = NULL,
                          type = NULL) {
  if (length(levels) == 1) {
    type <- "regression"
    obj_class <- "cal_regression"
  } else if (length(levels) == 2) {
    if (is.null(type)) {
      type <- "binary"
    }
    obj_class <- "cal_binary"
  } else if (length(levels) > 2) {
    if (is.null(type)) {
      type <- "one_vs_all"
    }
    obj_class <- "cal_multi"
  } else {
    cli::cli_abort("Cannot translate {.arg levels} to a class.")
  }

  structure(
    list(
      type = type,
      method = method,
      truth = truth,
      levels = levels,
      rows = rows,
      source_class = source_class,
      estimates = estimate
    ),
    class = c(additional_classes, obj_class, "cal_object")
  )
}

# Wraps tidyselect call to avoid code duplication in the function above
tidyselect_cols <- function(.data, x) {
  tidyselect::eval_select(
    expr = enquo(x),
    data = .data[unique(names(.data))],
    allow_rename = FALSE
  )
}

# dplyr::group_map() does not pass the parent function's `...`, it overrides it
# and there seems to be no way to change it. This function will split the the
# data set by all the combination of the grouped variables. It will respect
# any tidyeval variable calls made prior to calling the calibration
split_dplyr_groups <- function(.data) {
  if (dplyr::is_grouped_df(.data)) {
    grp_keys <- .data |> dplyr::group_keys()
    grp_keys <- purrr::map(grp_keys, as.character)
    grp_var <- .data |> dplyr::group_vars()
    grp_data <- .data |> tidyr::nest()
    grp_filters <- purrr::map(grp_keys[[1]], ~ expr(!!parse_expr(grp_var) == !!.x))
    grp_n <- purrr::map_int(grp_data$data, nrow)
    res <- vector(mode = "list", length = length(grp_filters))
    for (i in seq_along(res)) {
      res[[i]]$data <- grp_data$data[[i]]
      res[[i]]$filter <- grp_filters[[i]]
      res[[i]]$rows <- grp_n[[i]]
    }
  } else {
    res <- list(list(data = .data))
  }
  res
}

stop_null_parameters <- function(x) {
  if (!is.null(x)) {
    cli::cli_abort("The {.arg parameters} argument is only valid for {.code tune_results}.")
  }
}

make_group_df <- function(predictions, group) {
  if (length(group) > 0) {
    grp_df <- predictions[group]
    # TODO check for zero variance and clean if needed
    if (length(group) > 1) {
      cli::cli_abort(
        c(
          x = "{.arg .by} cannot select more than one column.",
          i = "The following columns were selected:",
          i = "{group}"
        ),
        call = NULL
      )
    }
  } else {
    grp_df <- dplyr::tibble(group = rep(1, nrow(predictions)))
  }
  grp_df
}

make_cal_filters <- function(key) {
  unq_vals <- purrr::map_int(key, vctrs::vec_unique_count)
  zv <- unq_vals == 1
  if (any(zv)) {
    key <- key[, !zv]
  }

  nm_chr <- names(key)
  num_combo <- nrow(key)
  num_vars <- ncol(key)
  # if (num_vars > 1) {
  #   cli::cli_abort(
  #     "Only a single grouping columns is currently supported.",
  #     call = FALSE
  #   )
  # }

  if (num_combo == 0 | num_vars == 0) {
    return(list(NULL))
  }

  res <- vector(mode = "list", length = num_combo)
  for (i in seq_len(num_vars)) {
    gvar <- nm_chr[[i]]
    tmp <- purrr::map(
      key[[i]],
      ~ rlang::expr(!!rlang::parse_expr(gvar) == !!.x)
    )

    if (i == 1) {
      res <- tmp
    } else {
      res <- purrr::map2(res, tmp, ~  rlang::expr(!!.x & !!.y))
    }
  }

  res
}

check_req_pkgs <- function(x, unsmooth = character(0)) {
  gam_mod <- purrr::map_lgl(x$estimates, ~ inherits(.x$estimate, "gam"))
  if (any(gam_mod)) {
    res <- c("mgcv", "probably")
  } else {
    res <- c(unsmooth, "probably")
  }
  res
}

# ------------------------------- GAM Helpers ----------------------------------

f_from_str <- function(y, x, smooth = FALSE) {
  if (smooth) {
    x <- paste0("s(", x, ")")
  }
  trms <- paste0(x, collapse = "+")
  f <- paste(y, "~", trms)
  f <- stats::as.formula(f)
  attr(f, ".Environment") <- rlang::base_env()
  f
}

# mgcv multinomial models needs a list of formulas, one for each level, and
# only the first one requires a LHS
multinomial_f_from_str <- function(y, x) {
  num_class <- length(x)
  res <- vector(mode = "list", length = num_class - 1)
  for (i in seq_along(res)) {
    if (i == 1) {
      res[[i]] <- f_from_str(y, x[-length(x)], smooth = TRUE)
    } else {
      res[[i]] <- f_from_str(NULL, x[-length(x)], smooth = TRUE)
    }
  }
  res
}

turn_off_smooth_if_too_few_unique <- function(.data, estimate, smooth, min_vals = 10) {
  predictors <- .data[, estimate]
  if (smooth) {
    n_unique <- purrr::map_int(predictors, vctrs::vec_unique_count)
    if (min(n_unique) < min_vals) {
      smooth <- FALSE
      cli::cli_warn(
        "Too few unique observations for spline-based calibrator.
        Setting {.code smooth = FALSE}."
      )
    }
  }
  smooth
}

# ------------------------------ 1 versus all helpers --------------------------

fit_over_classes <- function(.fn, .data, truth, estimate, ...) {
  lvls <- levels(.data[[ truth ]])
  prob_cols <- estimate

  res <- purrr::map2(
    lvls,
    estimate,
    fit_1_vs_all,
    .fn,
    .data = .data,
    truth = truth,
    estimate = estimate,
    ...
  )
  names(res) <- lvls
  res
}

fit_1_vs_all <- function(class, prob_col, .fn, .data, truth, estimate, ...) {

  # Redefine the outcome class as the current class level
  outcome <- .data[[ truth ]]
  new_class <- ifelse(outcome == class, class, ".other")
  new_class <- factor(new_class, levels = c(class, ".other"))
  .data[[ truth ]] <- new_class

  res <- .fn(.data, truth = truth, estimate = prob_col, ...)
  res
}
