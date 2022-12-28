#-------------------------- Binary Objects -------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  print_cal_binary(x, ...)
}

#' @export
print.cal_estimate_isotonic <- function(x, ...) {
  print_cal_binary(x, upv = TRUE, ...)
}

print_cal_binary <- function(x, upv = FALSE, ...) {
  cli::cli_div(theme = list(
    span.val0 = list(color = "blue"),
    span.val1 = list(color = "yellow"),
    span.val2 = list(color = "darkgreen")
  ))
  rows <- prettyNum(x$rows, ",")
  cli::cli_h3("Probability Calibration")
  cli::cli_text("Method: {.val2 {x$method}}")
  cli::cli_text("Type: {.val2 Binary}")
  cli::cli_text("Train set size: {.val2 {rows}}")
  if (upv) {
    upv_no <- prettyNum(nrow(x$estimates[[1]]$estimate[[1]]), ",")
    cli::cli_text("Unique Probability Values: {.val2 {upv_no}}")
  }
  cli::cli_text("Truth variable: `{.val0 {x$truth}}`")
  cli::cli_text("Estimate variables:")
  cli::cli_text("{.val1 `{x$levels[[1]]}`} ==> {.val0 {names(x$levels[1])}}")
  cli::cli_text("{.val1 `{x$levels[[2]]}`} ==> {.val0 {names(x$levels[2])}}")
  cli::cli_end()
}

as_binary_cal_object <- function(estimate,
                                 truth,
                                 levels,
                                 method,
                                 additional_class = NULL,
                                 rows) {
  truth_name <- as_name(enquo(truth))

  structure(
    list(
      type = "binary",
      method = method,
      truth = truth_name,
      levels = levels,
      rows = rows,
      estimates = estimate
    ),
    class = c(additional_class, "cal_binary", "cal_object")
  )
}

# ------------------------------- Utils ----------------------------------------

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

  truth_levels <- levels(.data[[truth_str]])

  estimate_str <- tidyselect_cols(.data, {{ estimate }}) %>%
    names()

  if (length(estimate_str) == 0) {
    cli::cli_abort("{.arg estimate} must select at least one column.")
  }

  if (all(substr(estimate_str, 1, 6) == ".pred_")) {
    est_map <- purrr::map(
      truth_levels,
      ~ sym(estimate_str[paste0(".pred_", .x) == estimate_str])
    )
  } else {
    est_map <- purrr::map(
      seq_along(truth_levels),
      ~ sym(estimate_str[[.x]])
    )
  }

  set_names(est_map, truth_levels)
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
        list(
          data = .data %>%
            dplyr::filter(, !!.x) %>%
            dplyr::ungroup(),
          filter = .x
        )
      })
  } else {
    list(list(data = .data))
  }
}

stop_null_parameters <- function(x) {
  if(!is.null(x)) {
    rlang::abort("The `parameters` argument is only valid for `tune_results`.")
  }
}
