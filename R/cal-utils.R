# Centralizes the figuring out of which probability-variable maps to which
# factor level of the "truth" variable. This is where the logic of finding
# and mapping tidymodels explicit column names happen. If there are no .pred_
# named variables, it will map the variables based on the position.
# It returns a named list, wit the variable names as syms, and the assigned
# levels as the name.
truth_estimate_map <- function(.data, truth, estimate, validate = FALSE) {
  truth_str <- tidyselect_cols(.data, {{ truth }})

  if (is.integer(truth_str)) {
    truth_str <- names(truth_str)
  }

  # Get the name(s) of the column(s) that have the predicted values. For binary
  # data, this is a single column name.
  estimate_str <- .data |>
    tidyselect_cols({{ estimate }}) |>
    names()

  if (length(estimate_str) == 0) {
    cli::cli_abort("{.arg estimate} must select at least one column.")
  }

  truth_levels <- levels(.data[[truth_str]])

  # `est_map` maps the levels of the outcome to the corresponding column(s) in
  # the data
  if (length(truth_levels) > 0) {
    if (all(substr(estimate_str, 1, 6) == ".pred_")) {
      est_map <- purrr::map(
        truth_levels,
        ~ {
          match <- paste0(".pred_", .x) == estimate_str
          if (any(match)) {
            sym(estimate_str[match])
          }
        }
      )
    } else {
      if (length(estimate_str) == 1) {
        est_map <- list(sym(estimate_str), NULL)
      } else {
        est_map <- purrr::map(seq_along(truth_levels), ~ sym(estimate_str[[.x]]))
      }
    }
    if (validate) {
      check_level_consistency(truth_levels, est_map)
    }
    res <- set_names(est_map, truth_levels)
  } else {
    # regression case
    res <- list(sym(estimate_str))
    names(res) <- "predictions"
  }
  purrr::discard(res, is.null)
}

check_level_consistency <- function(lvls, mapping) {
  null_map <- purrr::map_lgl(mapping, is_null)
  if (any(null_map) | length(lvls) != length(mapping)) {
    missings <- lvls[null_map]
    missings <- paste0(missings, collapse = ", ")
    cols <- mapping[!null_map]
    cols <- purrr::map_chr(cols, as.character)
    cols <- paste0(cols, collapse = ", ")
    cli::cli_abort(
      c(
        "We can't connect the specified prediction columns to {.val {missings}}.",
        "i" = "The selected columns were {.val {cols}}.",
        "i" = "Are there more columns to add in the function call?"
      )
    )
  }
  invisible(NULL)
}

nm_levels <- function(x) {
  purrr::map_chr(x, rlang::as_name)
}
