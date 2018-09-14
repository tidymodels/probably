# ------------------------------------------------------------------------------
# Printing

# Abbreviation used in tibbles and str() (through that, rstudio)
vec_ptype_abbr.class_pred <- function(x) {
  "cpred"
}

# ------------------------------------------------------------------------------
# Casting

# -----------------------
# Required casts

#' @export
#' @method vec_cast class_pred
#' @export vec_cast.class_pred
#' @importFrom vctrs vec_cast
vec_cast.class_pred <- function(x, to) UseMethod("vec_cast.class_pred")

#' @method vec_cast.class_pred default
#' @export
#' @importFrom vctrs stop_incompatible_cast
vec_cast.class_pred.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @method vec_cast.class_pred NULL
#' @export
vec_cast.class_pred.NULL <- function(x, to) {
  x
}

#' @export
vec_cast.NULL.class_pred <- function(x, to) {
  x
}

#' @method vec_cast.class_pred class_pred
#' @export
vec_cast.class_pred.class_pred <- function(x, to) {
  x
}

# -----------------------
# Custom casts

# factor -> class_pred, assume no equivocal values

#' @method vec_cast.class_pred factor
#' @export
vec_cast.class_pred.factor <- function(x, to) {
  class_pred(x, integer())
}

# class_pred -> factor, equivocals become NAs

#' @export
#' @importFrom vctrs vec_data
vec_cast.factor.class_pred <- function(x, to) {

  x_data <- vec_data(x)
  labs <- attr(x, "labels")

  x_data[is_equivocal(x)] <- NA_integer_

  factor(x_data, labels = labs)
}

# ------------------------------------------------------------------------------
# Coercion

# Required coercion

#' @export
#' @method vec_type2 class_pred
#' @export vec_type2.class_pred
#' @importFrom vctrs vec_type2
vec_type2.class_pred <- function(x, y) {
  UseMethod("vec_type2.class_pred")
}

#' @method vec_type2.class_pred default
#' @export
#' @importFrom vctrs stop_incompatible_type
vec_type2.class_pred.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @method vec_type2.class_pred NULL
#' @export
vec_type2.class_pred.NULL <- function(x, y) {
  class_pred()
}

#' @export
vec_type2.NULL.class_pred <- function(x, y) {
  class_pred()
}

# Custom coercion

# class_pred + class_pred = class_pred with unioned labels

#' @method vec_type2.class_pred class_pred
#' @export
vec_type2.class_pred.class_pred <- function(x, y) {
  new_class_pred(integer(), labels = union_labels(x, y))
}

union_labels <- function(x, y) {
  x_labels <- attr(x, "labels")
  y_labels <- attr(y, "labels")

  sort(union(x_labels, y_labels))
}
