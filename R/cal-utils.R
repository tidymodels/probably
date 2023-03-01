# Centralizes the figuring out of which probability-variable maps to which
# factor level of the "truth" variable. This is where the logic of finding
# and mapping tidymodels explicit column names happen. If there are no .pred_
# named variables, it will map the variables based on the position.
# It returns a named list, wit the variable names as syms, and the assigned
# levels as the name.
truth_estimate_map <- function(.data, truth, estimate) {
  truth_str <- tidyselect_cols(.data, {{ truth }})

  if(class(truth_str) == "integer") {
    truth_str <- names(truth_str)
  }

  estimate_str <- .data %>%
    tidyselect_cols({{ estimate }}) %>%
    names()

  if (length(estimate_str) == 0) {
    cli::cli_abort("{.arg estimate} must select at least one column.")
  }

  truth_levels <- levels(.data[[truth_str]])

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
      est_map <- purrr::map(
        seq_along(truth_levels),
        ~ sym(estimate_str[[.x]])
      )
    }

    res <- set_names(est_map, truth_levels)
  } else {
    res <- list(sym(estimate_str))
    names(res) <- "predictions"
  }
  purrr::discard(res, is.null)
}