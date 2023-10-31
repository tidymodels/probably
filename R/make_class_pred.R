# Make - multi class -----------------------------------------------------------

#' Create a `class_pred` vector from class probabilities
#'
#' These functions can be used to convert class probability estimates to
#'  `class_pred` objects with an optional equivocal zone.
#'
#' @param ... Numeric vectors corresponding to class probabilities. There should
#' be one for each level in `levels`, and _it is assumed that the vectors
#' are in the same order as `levels`_.
#'
#' @param estimate A single numeric vector corresponding to the class
#' probabilities of the first level in `levels`.
#'
#' @param levels A character vector of class levels. The length should be the
#' same as the number of selections made through `...`, or length `2`
#' for `make_two_class_pred()`.
#'
#' @param ordered A single logical to determine if the levels should be regarded
#'  as ordered (in the order given). This results in a `class_pred` object
#'  that is flagged as ordered.
#'
#' @param min_prob A single numeric value. If any probabilities are less than
#'  this value (by row), the row is marked as _equivocal_.
#'
#' @param threshold A single numeric value for the threshold to call a row to
#'  be labeled as the first value of `levels`.
#'
#' @param buffer A numeric vector of length 1 or 2 for the buffer around
#' `threshold` that defines the equivocal zone (i.e., `threshold - buffer[1]` to
#'  `threshold + buffer[2]`). A length 1 vector is recycled to length 2. The
#'  default, `NULL`, is interpreted as no equivocal zone.
#'
#' @return A vector of class [`class_pred`].
#'
#' @examples
#'
#' library(dplyr)
#'
#' good <- segment_logistic$.pred_good
#' lvls <- levels(segment_logistic$Class)
#'
#' # Equivocal zone of .5 +/- .15
#' make_two_class_pred(good, lvls, buffer = 0.15)
#'
#' # Equivocal zone of c(.5 - .05, .5 + .15)
#' make_two_class_pred(good, lvls, buffer = c(0.05, 0.15))
#'
#' # These functions are useful alongside dplyr::mutate()
#' segment_logistic %>%
#'   mutate(
#'     .class_pred = make_two_class_pred(
#'       estimate = .pred_good,
#'       levels = levels(Class),
#'       buffer = 0.15
#'     )
#'   )
#'
#' # Multi-class example
#' # Note that we provide class probability columns in the same
#' # order as the levels
#' species_probs %>%
#'   mutate(
#'     .class_pred = make_class_pred(
#'       .pred_bobcat, .pred_coyote, .pred_gray_fox,
#'       levels = levels(Species),
#'       min_prob = .5
#'     )
#'   )
#'
#' @export
make_class_pred <- function(...,
                            levels,
                            ordered = FALSE,
                            min_prob = 1 / length(levels)) {
  dots <- rlang::quos(...)
  probs <- lapply(dots, rlang::eval_tidy)

  # Length check
  lens <- vapply(probs, length, numeric(1))
  if (any(lens != lens[1])) {
    cli::cli_abort(
      "All vectors passed to {.arg ...} must be of the same length."
    )
  }

  # Type check
  num_cols <- vapply(probs, is.numeric, logical(1))
  if (any(!num_cols)) {
    not_numeric <- which(!num_cols)
    cli::cli_abort(
      c(
        "x" = "The index supplied to `...` are not numeric:",
        "i" = "{not_numeric}"
      )
    )
  }

  # Levels check (length and type)
  if (length(levels) != length(probs) || !is.character(levels)) {
    cli::cli_abort(
      "{.arg levels} must be a character vector with the \\
      same length as the number of vectors passed to {.arg ...}."
    )
  }

  # min_prob checks
  if (length(min_prob) != 1 && is.numeric(min_prob)) {
    cli::cli_abort("{.arg min_prob} must be a single numeric value.")
  }

  probs <- list2mat(probs)

  x <- levels[apply(probs, 1, which.max)]
  x <- factor(x, levels = levels, ordered = ordered)

  if (!is.null(min_prob)) {
    eq_ind <- which(apply(probs, 1, max) < min_prob)
  } else {
    eq_ind <- integer()
  }

  x <- class_pred(x, eq_ind)

  x
}

# Make - two class -------------------------------------------------------------

#' @rdname make_class_pred
#' @export
make_two_class_pred <- function(estimate,
                                levels,
                                threshold = 0.5,
                                ordered = FALSE,
                                buffer = NULL) {
  if (length(levels) != 2 || !is.character(levels)) {
    cli::cli_abort("{.arg levels} must be a character vector of length 2.")
  }

  if (!is.numeric(estimate)) {
    cli::cli_abort("The selected probability vector should be numeric.")
  }

  if (length(buffer) > 2 && is.numeric(buffer)) {
    cli::cli_abort("{.arg buffer} must be a numeric vector of length 1 or 2.")
  }

  if (length(buffer) == 1) {
    buffer <- c(buffer, buffer)
  }

  x <- ifelse(estimate >= threshold, levels[1], levels[2])
  x <- factor(x, levels = levels, ordered = ordered)

  if (is.null(buffer)) {
    eq_ind <- integer()
  } else {
    eq_ind <- which(
      estimate >= threshold - buffer[1] &
        estimate <= threshold + buffer[2]
    )
  }

  x <- class_pred(x, eq_ind)

  x
}

# Append -----------------------------------------------------------------------

#' Add a `class_pred` column
#'
#' This function is similar to [make_class_pred()], but is useful when you have
#' a large number of class probability columns and want to use `tidyselect`
#' helpers. It appends the new `class_pred` vector as a column on the original
#' data frame.
#'
#' @inheritParams make_class_pred
#'
#' @param .data A data frame or tibble.
#'
#' @param ... One or more unquoted expressions separated by commas
#'  to capture the columns of `.data` containing the class
#'  probabilities. You can treat variable names like they are
#'  positions, so you can use expressions like `x:y` to select ranges
#'  of variables or use selector functions to choose which columns.
#'  For `make_class_pred`, the columns for all class probabilities
#'  should be selected (in the same order as the `levels` object).
#'  For `two_class_pred`, a vector of class probabilities should be
#'  selected.
#'
#' @param name A single character value for the name of the appended
#'  `class_pred` column.
#'
#' @return `.data` with an extra `class_pred` column appended onto it.
#'
#' @examples
#'
#' # The following two examples are equivalent and demonstrate
#' # the helper, append_class_pred()
#'
#' library(dplyr)
#'
#' species_probs %>%
#'   mutate(
#'     .class_pred = make_class_pred(
#'       .pred_bobcat, .pred_coyote, .pred_gray_fox,
#'       levels = levels(Species),
#'       min_prob = .5
#'     )
#'   )
#'
#' lvls <- levels(species_probs$Species)
#'
#' append_class_pred(
#'   .data = species_probs,
#'   contains(".pred_"),
#'   levels = lvls,
#'   min_prob = .5
#' )
#'
#' @export
append_class_pred <- function(.data,
                              ...,
                              levels,
                              ordered = FALSE,
                              min_prob = 1 / length(levels),
                              name = ".class_pred") {
  if (!is.data.frame(.data) && ncol(.data) < 2) {
    cli::cli_abort(
      "{.arg .data} should be a data frame or tibble with at least 2 columns."
    )
  }

  if (!rlang::is_scalar_character(name)) {
    cli::cli_abort("{.arg name} must be a single character value.")
  }

  sel <- tidyselect::eval_select(
    expr = expr(c(...)),
    data = .data
  )

  prob_names <- names(sel)

  if (length(prob_names) < 2) {
    cli::cli_abort("{.arg ...} should select at least 2 columns.")
  }

  prob_syms <- rlang::syms(prob_names)

  # Using a mutate() automatically supports groups
  dplyr::mutate(
    .data,
    !!name := make_class_pred(
      !!!prob_syms,
      levels = levels,
      ordered = ordered,
      min_prob = min_prob
    )
  )
}

# Util -------------------------------------------------------------------------

list2mat <- function(lst) {
  n_col <- length(lst)
  vec <- unlist(lst, recursive = FALSE, use.names = FALSE)
  matrix(vec, ncol = n_col)
}
