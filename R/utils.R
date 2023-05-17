
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
  msg <- paste0("No implementation of `", fn, "()` for object of class ", cls, ".")
  abort(msg)
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
