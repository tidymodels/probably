
# ------------------------------------------------------------------------------
# Creation

#' @importFrom vctrs new_vctr
new_class_pred <- function(x, labels, ..., subclass = NULL) {

  stopifnot(is.integer(x))
  stopifnot(is.character(labels))

  new_vctr(
    .data = x,
    labels = labels,
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
#' By default, `probably` prints equivocal values as `EQ`. You can change this
#' with the global option, `probably.equivocal_label`.
#'
#' @param x A factor or ordered factor.
#' @param which An integer vector specifying the locations of `x` to declare
#' as equivocal.
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
#' @export
#' @importFrom rlang abort
#' @importFrom vctrs vec_cast
class_pred <- function(x = factor(), which = integer()) {

  # Check invariants
  if(!is.factor(x)) {
    abort("`x` must be a factor.")
  }

  if(!is.numeric(which)) {
    abort("`which` must be a numeric.")
  }

  # which can be double, but convert to integer and warn about
  # any lossy conversion
  which <- vec_cast(which, integer())

  # no duplicates allowed
  which <- unique(which)

  # # more checks on values of which with regard to length of x?
  # if(max(which) > length(x)) stop("The largest value of which can be length(x)")

  labs <- levels(x)

  # Check for `EQ` in labels. Not allowed.
  eq <- probably.equivocal_label
  if(eq %in% labs) {
    msg <- paste0("`\"", eq, "\"` is reserved for equivocal values and must not already be a level.")
    abort(msg)
  }

  # rip out the underlying integer structure
  # as_integer() also removes attributes
  x_int <- rlang::as_integer(unclass(x))

  # declare equivocal
  x_int[which] <- 0L

  new_class_pred(
    x = x_int,
    labels = labs
  )
}

# ------------------------------------------------------------------------------
# Printing

# Always return a character vector
# Rely on as.character.factor() for NA handling
# Used by data.frame() columns and general printing
#' @export
format.class_pred <- function(x, ...) {

  x <- format_as_factor(x)

  out <- as.character.factor(x)

  out
}

# This is different from as_factor()
# This changes enough to be able to print right
#' @importFrom vctrs vec_data
format_as_factor <- function(x, ...) {

  lab_equivocal <- paste0("[", probably.equivocal_label, "]")
  labs_known <- attr(x, "labels")

  # In this order b/c `0 = equivocal`
  labs <- c(lab_equivocal, labs_known)

  # strip attributes
  x <- vec_data(x)

  # Factors start at 1
  x <- x + 1L

  attr(x, "levels") <- labs
  class(x) <- "factor"

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
as_class_pred <- function(x, which = integer()) {
  UseMethod("as_class_pred")
}

#' @export
as_class_pred.default <- function(x, which = integer()) {
  abort_default(x, "as_class_pred")
}

#' @export
as_class_pred.factor <- function(x, which = integer()) {
  class_pred(x, which)
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
  is_0 <- vec_data(x) == 0L

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
#' @importFrom vctrs vec_data
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
#' @inheritParams is_equivocal
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
