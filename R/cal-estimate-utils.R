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
  cli::cli_text("Truth variable: `{.val0 {x$truth}}`")
  cli::cli_text("Estimate variable: {.val1 `{x$levels[[1]]}`}")

  cli::cli_end()
}

# ------------------------ Estimate name methods -------------------------------

cal_class_name <- function(x) {
  UseMethod("cal_class_name")
}

cal_class_name.data.frame <- function(x) {
  "Data Frame"
}

cal_class_name.tune_results <- function(x) {
  "Tune Results"
}

cal_class_name.tune_results <- function(x) {
  "Tune Results"
}

cal_class_name.rset <- function(x) {
  "Resampled data set"
}

# ------------------------------- Utils ----------------------------------------

as_regression_cal_object <- function(estimate,
                                     truth,
                                     levels,
                                     method,
                                     rows,
                                     additional_class = NULL,
                                     source_class = NULL) {
  truth <- enquo(truth)

  as_cal_object(
    estimate = estimate,
    truth = !!truth,
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
    rlang::abort("Can't translate 'levels' to a class.")
  }

  str_truth <- as_name(enquo(truth))

  structure(
    list(
      type = type,
      method = method,
      truth = str_truth,
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
    grp_keys <- .data %>% dplyr::group_keys()
    grp_keys <- purrr::map(grp_keys, as.character)
    grp_var <- .data %>% dplyr::group_vars()
    grp_data <- .data %>% tidyr::nest()
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

create_filter_expr <- function(...) {
  purrr::imap(..., ~ expr(!!parse_expr(.y) == !!.x)) %>%
    purrr::reduce(function(x, y) expr(!!x & !!y))
}

stop_null_parameters <- function(x) {
  if (!is.null(x)) {
    rlang::abort("The `parameters` argument is only valid for `tune_results`.")
  }
}
