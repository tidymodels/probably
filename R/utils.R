# is there a forcats for this?
recode_data <- function(obs, prob, threshold, event_level) {
  lvl <- levels(obs)
  if (identical(event_level, "first")) {
    pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  } else {
    pred <- ifelse(prob >= threshold, lvl[2], lvl[1])
  }
  factor(pred, levels = lvl)
}

quote_collapse <- function(x, quote = "`", collapse = ", ") {
  paste(encodeString(x, quote = quote), collapse = collapse)
}

abort_default <- function(x, fn) {
  cls <- quote_collapse(class(x))
  cli::cli_abort("No implementation of {.fn {fn}} for {.obj_type_friendly {cls}}.")
}

# Check if a class_pred object came from an ordered factor
is_ordered_class_pred <- function(x) {
  attr(x, "ordered")
}

get_equivocal_label <- function(x) {
  attr(x, "equivocal")
}

is_ordered <- function(x) {
  UseMethod("is_ordered")
}

# Must export internal methods for testing
#' @export
is_ordered.class_pred <- function(x) {
  is_ordered_class_pred(x)
}

# Must export internal methods for testing
#' @export
is_ordered.default <- function(x) {
  is.ordered(x)
}

get_group_argument <- function(group, .data, call = rlang::env_parent()) {
  group <- rlang::enquo(group)

  group_names <- tidyselect::eval_select(
    expr = group,
    data = .data,
    allow_rename = FALSE,
    allow_empty = TRUE,
    allow_predicates = TRUE,
    error_call = call
  )

  n_group_names <- length(group_names)

  useable_config <- n_group_names == 0 &&
    ".config" %in% names(.data) &&
    dplyr::n_distinct(.data[[".config"]]) > 1

  if (useable_config) {
    return(quo(.config))
  }

  if (n_group_names > 1) {
    cli::cli_abort(
      c(
        x = "{.arg .by} cannot select more than one column.",
        i = "The following {n_group_names} columns were selected:",
        i = "{names(group_names)}"
      )
    )
  }

  return(group)
}

abort_if_tune_result <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "This function can only be used with an {.cls rset} object or the \\
       results of {.fn tune::fit_resamples} with a {.field .predictions} \\
       column.",
      i = "Not an {.cls tune_results} object."
    ),
    call = call
  )
}

abort_if_grouped_df <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "x" = "This function does not work with grouped data frames.",
      "i" = "Apply {.fn dplyr::ungroup} and use the {.arg .by} argument."
    ),
    call = call
  )
}

turn_off_smooth_if_too_few_unique <- function(.data, estimate, smooth) {
  if (smooth) {
    estimate_name <- names(tidyselect::eval_select(estimate, .data))
    estimate_name <- estimate_name[length(estimate_name)]

    if (inherits(.data, "grouped_df")) {
      n_unique <- .data |> 
        dplyr::summarise(
          dplyr::across(dplyr::all_of(estimate_name), dplyr::n_distinct)
        ) |>
        dplyr::pull(dplyr::all_of(estimate_name)) |>
        min()
    } else {
      n_unique <- dplyr::n_distinct(
        dplyr::pull(.data, dplyr::all_of(estimate_name))
      )
    }

    if (n_unique < 10) {
      smooth <- FALSE
      cli::cli_warn(
        "Too few unique observations for spline-based calibrator.
        Setting {.code smooth = FALSE}."
      )
    }
  }
  smooth
}