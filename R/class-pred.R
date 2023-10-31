# ------------------------------------------------------------------------------
# Creation

new_class_pred <- function(x, labels, ordered = FALSE, equivocal = "[EQ]", ..., subclass = NULL) {
  stopifnot(is.integer(x))
  stopifnot(is.character(labels))
  stopifnot(is.logical(ordered) && length(ordered) == 1L)
  stopifnot(is.character(equivocal) && length(equivocal) == 1L)

  new_vctr(
    .data = x,
    labels = labels,
    ordered = ordered,
    equivocal = equivocal,
    ...,
    class = c(subclass, "class_pred")
  )
}

#' Create a class prediction object
#'
#' `class_pred()` creates a `class_pred` object from a factor or ordered
#' factor. You can optionally specify values of the factor to be set
#' as _equivocal_.
#'
#' Equivocal values are those that you feel unsure about, and would like to
#' exclude from performance calculations or other metrics.
#'
#' @param x A factor or ordered factor.
#' @param which An integer vector specifying the locations of `x` to declare
#' as equivocal.
#' @param equivocal A single character specifying the equivocal label used
#' when printing.
#'
#' @examples
#'
#' x <- factor(c("Yes", "No", "Yes", "Yes"))
#'
#' # Create a class_pred object from a factor
#' class_pred(x)
#'
#' # Say you aren't sure about that 2nd "Yes" value. You could mark it as
#' # equivocal.
#' class_pred(x, which = 3)
#'
#' # Maybe you want a different equivocal label
#' class_pred(x, which = 3, equivocal = "eq_value")
#'
#' @export
class_pred <- function(x = factor(), which = integer(), equivocal = "[EQ]") {
  # Check invariants
  if (!is.factor(x)) {
    abort("`x` must be a factor.")
  }

  if (!is.numeric(which)) {
    abort("`which` must be a numeric.")
  }

  if (!rlang::is_scalar_character(equivocal)) {
    abort("`equivocal` must be a length 1 character.")
  }

  # which can be double, but convert to integer and warn about
  # any lossy conversion
  which <- vec_cast(which, integer())

  # no duplicates allowed
  which <- unique(which)

  # which cannot go outside the range of the number of values in x
  if (length(which) > 0L && max(which) > length(x)) {
    msg <- paste0("The largest value of `which` can be ", length(x), ".")
    abort(msg)
  }

  labs <- levels(x)

  # Check for `equivocal` in labels. Not allowed.
  if (equivocal %in% labs) {
    msg <- paste0(
      "`\"", equivocal, "\"`",
      "is reserved for equivocal values",
      "and must not already be a level."
    )
    abort(msg)
  }

  # rip out the underlying integer structure
  # as.integer() also removes attributes
  x_int <- as.integer(unclass(x))

  # declare equivocal
  x_int[which] <- 0L

  new_class_pred(
    x = x_int,
    labels = labs,
    ordered = is.ordered(x),
    equivocal = equivocal
  )
}

# ------------------------------------------------------------------------------
# Printing

# Always return a character vector
# Rely on as.character.factor() for NA handling
# Used by data.frame() columns and general printing
#' @export
format.class_pred <- function(x, ...) {
  eq_lgl <- is_equivocal(x)
  eq_lbl <- get_equivocal_label(x)
  x <- as.character(x)
  x[eq_lgl] <- eq_lbl
  x
}

# ------------------------------------------------------------------------------
# Coercion

#' Coerce to a `class_pred` object
#'
#' `as_class_pred()` provides coercion to `class_pred` from other
#' existing objects.
#'
#' @inheritParams class_pred
#'
#' @examples
#'
#' x <- factor(c("Yes", "No", "Yes", "Yes"))
#' as_class_pred(x)
#'
#' @export
as_class_pred <- function(x, which = integer(), equivocal = "[EQ]") {
  UseMethod("as_class_pred")
}

#' @export
as_class_pred.default <- function(x, which = integer(), equivocal = "[EQ]") {
  abort_default(x, "as_class_pred")
}

#' @export
as_class_pred.factor <- function(x, which = integer(), equivocal = "[EQ]") {
  class_pred(x, which, equivocal)
}

# ------------------------------------------------------------------------------
# Methods

# -----------------------
# is_equivocal

#' Locate equivocal values
#'
#' These functions provide multiple methods of checking for equivocal values,
#' and finding their locations.
#'
#' @param x A `class_pred` object.
#'
#' @return
#'
#' `is_equivocal()` returns a logical vector the same length as `x`
#' where `TRUE` means the value is equivocal.
#'
#' `which_equivocal()` returns an integer vector specifying the locations
#' of the equivocal values.
#'
#' `any_equivocal()` returns `TRUE` if there are any equivocal values.
#'
#' @examples
#'
#' x <- class_pred(factor(1:10), which = c(2, 5))
#'
#' is_equivocal(x)
#'
#' which_equivocal(x)
#'
#' any_equivocal(x)
#'
#' @name locate-equivocal
#'
NULL

#' @rdname locate-equivocal
#' @export
is_equivocal <- function(x) {
  UseMethod("is_equivocal")
}

#' @export
is_equivocal.default <- function(x) {
  abort_default(x, "is_equivocal")
}

#' @export
is_equivocal.class_pred <- function(x) {
  is_0 <- unclass(x) == 0L

  # NA values are also FALSE
  is_0[is.na(is_0)] <- FALSE

  as.logical(is_0)
}

# -----------------------
# which_equivocal

#' @rdname locate-equivocal
#' @export
which_equivocal <- function(x) {
  UseMethod("which_equivocal")
}

#' @export
which_equivocal.default <- function(x) {
  abort_default(x, "which_equivocal")
}

#' @export
which_equivocal.class_pred <- function(x) {
  which(is_equivocal(x))
}

# -----------------------
# any_equivocal

#' @rdname locate-equivocal
#' @export
any_equivocal <- function(x) {
  UseMethod("any_equivocal")
}

#' @export
any_equivocal.default <- function(x) {
  abort_default(x, "any_equivocal")
}

#' @export
any_equivocal.class_pred <- function(x) {
  any(is_equivocal(x))
}

# -----------------------
# is_class_pred

#' Test if an object inherits from `class_pred`
#'
#' `is_class_pred()` checks if an object is a `class_pred` object.
#'
#' @param x An object.
#'
#' @examples
#'
#' x <- class_pred(factor(1:5))
#'
#' is_class_pred(x)
#'
#' @export
is_class_pred <- function(x) {
  inherits(x, "class_pred")
}

# -----------------------
# reportable_rate

#' Calculate the reportable rate
#'
#' The _reportable rate_ is defined as the percentage of class predictions
#' that are _not_ equivocal.
#'
#' The reportable rate is calculated as `(n_not_equivocal / n)`.
#'
#' @param x A `class_pred` object.
#'
#' @examples
#'
#' x <- class_pred(factor(1:5), which = c(1, 2))
#'
#' # 3 / 5
#' reportable_rate(x)
#'
#' @export
reportable_rate <- function(x) {
  UseMethod("reportable_rate")
}

#' @export
reportable_rate.default <- function(x) {
  abort_default(x, "reportable_rate")
}

#' @export
reportable_rate.class_pred <- function(x) {
  n <- length(x)

  if (n == 0L) {
    return(0L)
  }

  n_eq <- sum(is_equivocal(x))
  (n - n_eq) / n
}

# ------------------------------------------------------------------------------
# Base S3 Methods

#' Extract `class_pred` levels
#'
#' The levels of a `class_pred` object do _not_ include the equivocal value.
#'
#' @inheritParams is_equivocal
#'
#' @examples
#'
#' x <- class_pred(factor(1:5), which = 1)
#'
#' # notice that even though `1` is not in the `class_pred` vector, the
#' # level remains from the original factor
#' levels(x)
#'
#' @export
levels.class_pred <- function(x) {
  attr(x, "labels")
}
