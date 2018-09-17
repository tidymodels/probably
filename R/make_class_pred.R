#' Create a class_pred vector from class probabilities
#'
#' These functions can be used to convert class probability estimates to
#'  `class_pred` objects with an optional equivocal zone.
#'
#' @param .data A data frame or tibble.
#' @param ... One or more unquoted expressions separated by commas
#'  to capture the columns of `.data` containing the class
#'  probabilities. . You can treat variable names like they are
#'  positions, so you can use expressions like x:y to select ranges
#'  of variables or use selector functions to choose which columns.
#'  For `make_class_pred`, the columns for all class probabilities
#'  should be selected (in the same order as the `levels` object).
#'  For `two_class_pred`, a vector of class probabilities should be
#'  selected.
#' @param levels A character vector of class levels. For `make_class_pred`, the
#'  length should be the same as the number of columns selected by `...`.
#' @param ordered A single logical to determine if the levels should be regarded
#'  as ordered (in the order given).
#' @param min_prob A single numeric value. If any probabilities are less than
#'  this value (by row), the row is marked as _equivocal_.
#' @param threshold A single numeric value for the threshold to call a row to
#'  be labeled as the first value of `levels`.
#' @param buffer A single numeric value for the buffer around `threshold` that
#'  defines the equivocal zone (i.e., `threshold - buffer` to
#'  `threshold + buffer`).
#' @param range A numeric vector of length two that explicitly defines the
#'  equivocal zone.
#' @return A vector of class [`class_pred`].
#'
#' @examples
#' library(dplyr)
#' new_factor <-
#'   segment_logistic %>%
#'   two_class_pred(
#'     .pred_good,
#'     levels = levels(segment_logistic[["Class"]]),
#'     buffer = .15
#'   )
#'
#'
#' species_probs %>%
#'   mutate(
#'     new_pred =
#'       make_class_pred(
#'         .,
#'         starts_with(".pred_"),
#'         levels = levels(Species),
#'         min_prob = .5
#'       )
#'   )
#' @importFrom purrr map_lgl
#' @importFrom glue glue_collapse
#' @importFrom tidyselect vars_select
#' @export
make_class_pred <-
  function(.data,
           ...,
           levels,
           ordered = FALSE,
           min_prob = 1/length(levels)) {

    if (!is.data.frame(.data) && ncol(.data) < 2)
      stop ("`.data` should be a data frame or tibble with at least 2 columns.",
            call. = FALSE)

    prob_names <- tidyselect::vars_select(names(.data), !!!quos(...))
    if (length(prob_names) < 2)
      stop ("`...` should select at least 2 columns.", call. = FALSE)

    probs <- .data[, prob_names]

    num_cols <- purrr::map_lgl(probs, is.numeric)
    if (any(!num_cols))
      stop (
        "At least one column is not numeric: ",
        glue::glue_collapse(names(probs)[!num_cols], sep = ", ", last = " and ")
      )
    if (length(levels) != ncol(probs) && is.character(levels))
      stop ("`levels` must be a character vector with at least 2 levels.",
            call. = TRUE)

    if (length(min_prob) != 1 && is.numeric(min_prob))
      stop ("`min_prob` must be a single numeric value.", call. = TRUE)

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

#' @rdname make_class_pred
#' @export
two_class_pred <-
  function(.data,
           ...,
           levels,
           threshold = 0.5,
           ordered = FALSE,
           buffer = 0,
           range = NULL) {
    if (!is.data.frame(.data))
      stop ("`.data` should be a data frame or tibble.", call. = FALSE)

    prob_name <- tidyselect::vars_select(names(.data), !!!quos(...))
    if (length(prob_name) != 1)
      stop ("`...` should select a single column.", call. = FALSE)

    if (length(levels) != 2 && is.character(levels))
      stop ("`levels` must be a character vector of length 2.", call. = TRUE)
    if (!is.numeric(.data[[prob_name]]))
      stop ("The selected probability column should be numeric.", call. = FALSE)

    if (length(range) != 2 && is.numeric(range))
      stop ("`range` must be a numeric vector of length 2.", call. = TRUE)

    if (buffer != 0 & !is.null(range)) {
      stop("`buffer` and `range` are both specified; `range` will be used.",
           call. = FALSE)
      buffer <- 0
    }

    x <- ifelse(.data[[prob_name]] >= threshold, levels[1], levels[2])
    x <- factor(x, levels = levels, ordered = ordered)

    if (!is.null(range)) {
      range <- sort(range)
      eq_ind <-
        which(.data[[prob_name]] >= range[1] & .data[[prob_name]] <= range[2])
    } else {
      if (buffer != 0) {
        eq_ind <-
          which(
            .data[[prob_name]] >= threshold - buffer &
            .data[[prob_name]] <= threshold + buffer
          )
      } else {
        eq_ind <- integer()
      }
    }
    x <- class_pred(x, eq_ind)
    x
  }






