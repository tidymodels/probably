
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
  eq <- global_equivocal_label
  if(eq %in% labs) {
    abort("`\"", eq, "\"` is reserved for equivocal values and must not already be a level.")
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

  lab_equivocal <- paste0("[", global_equivocal_label, "]")
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

#' @export
as_class_pred <- function(x, which) {
  UseMethod("as_class_pred")
}

as_class_pred.default <- function(x, which) {
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

#' @export
is_class_pred <- function(x) {
  inherits(x, "class_pred")
}

# ------------------------------------------------------------------------------
# Base S3 Methods

#' @export
levels.class_pred <- function(x) {
  attr(x, "labels")
}
