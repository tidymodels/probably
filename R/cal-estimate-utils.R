#-------------------------- Binary Objects -------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  print_cls_cal(x, ...)
}

#' @export
print.cal_estimate_isotonic <- function(x, ...) {
  print_cls_cal(x, upv = TRUE, ...)
}

# ------------------------------- Multi ----------------------------------------

#' @export
print.cal_multi <- function(x, ...) {
  print_cls_cal(x, ...)
}


# ------------------------------- Regression -----------------------------------

#' @export
print.cal_regression <- function(x, ...) {
  print_reg_cal(x, ...)
}


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

# ------------------------------- Utils ----------------------------------------

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

stop_multiclass <- function() {
  cli::cli_abort("Multiclass not supported...yet")
}

# Centralizes the figuring out of which probability-variable maps to which
# factor level of the "truth" variable. This is where the logic of finding
# and mapping tidymodels explicit column names happen. If there are no .pred_
# named variables, it will map the variables based on the position.
# It returns a named list, wit the variable names as syms, and the assigned
# levels as the name.
truth_estimate_map <- function(.data, truth, estimate) {
  truth_str <- tidyselect_cols(.data, {{ truth }})

  estimate_str <- .data %>%
    tidyselect_cols({{ estimate }}) %>%
    names()

  if (length(estimate_str) == 0) {
    cli::cli_abort("{.arg estimate} must select at least one column.")
  }

  truth_levels <- levels(.data[[truth_str]])

  if (length(truth_levels) > 0) {
    if (all(substr(estimate_str, 1, 6) == ".pred_")) {
      res <- purrr::map(
        truth_levels,
        ~ {
          match <- paste0(".pred_", .x) == estimate_str
          if (any(match)) {
            sym(estimate_str[match])
          }
        }
      ) %>%
        set_names(truth_levels)
    } else {
      res <- purrr::map(
        seq_along(truth_levels),
        ~ {
          if (any(estimate_str == .x)) {
            sym(estimate_str[[.x]])
          }
        }
      ) %>%
        set_names(truth_levels)
    }
  } else {
    res <- list(sym(estimate_str))
    names(res) <- "predictions"
  }
  purrr::discard(res, is.null)
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
    .data %>%
      dplyr::summarise(.groups = "drop") %>%
      purrr::transpose() %>%
      purrr::map(~ {
        purrr::imap(.x, ~ expr(!!parse_expr(.y) == !!.x)) %>%
          purrr::reduce(function(x, y) expr(!!x & !!y))
      }) %>%
      purrr::map(~ {
        df <- .data %>%
          dplyr::filter(, !!.x) %>%
          dplyr::ungroup()

        list(
          data = df,
          filter = .x,
          rows = nrow(df)
        )
      })
  } else {
    list(list(data = .data))
  }
}

stop_null_parameters <- function(x) {
  if (!is.null(x)) {
    rlang::abort("The `parameters` argument is only valid for `tune_results`.")
  }
}

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
