# ------------------------------------------------------------------------------
# Reexports - from `generics`

# these reexports are necessary so we have as.factor() overloaded.
# this way, as.factor() calls vec_cast() so we get the right behavior from vctrs.
# This works because vctrs has as.factor.vctrs_vctr() and suggests `generics`

#' @importFrom generics as.factor
#' @export
generics::as.factor

#' @importFrom generics as.ordered
#' @export
generics::as.ordered

# ------------------------------------------------------------------------------
# Printing

# Abbreviation used in tibbles and str() (through that, rstudio)

#' @export
#' @importFrom vctrs vec_ptype_abbr
vec_ptype_abbr.class_pred <- function(x) {
  "clss_prd"
}

#' @export
#' @importFrom vctrs vec_print_header
vec_print_header.class_pred <- function(x) {
  # no header
}

#' @export
#' @importFrom vctrs vec_print_data
vec_print_data.class_pred <- function(x) {
  print(format_as_factor(x), max.levels = 0)
}

#' @export
#' @importFrom vctrs vec_print_footer
vec_print_footer.class_pred <- function(x) {
  cat_levels(x)
  cat_eq_count(x)
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
#' @importFrom vctrs warn_lossy_cast
vec_cast.factor.class_pred <- function(x, to) {

  if(any_equivocal(x)) {
    warn_lossy_cast(x, to, locations = which_equivocal(x))
  }

  x_data <- vec_data(x)
  labs <- attr(x, "labels")

  x_data[is_equivocal(x)] <- NA_integer_

  if(is.ordered(to)) {
    constructor <- ordered
  } else {
    constructor <- factor
  }

  # Specify levels as full sequence along labels in case the
  # data is missing a level from equivocal masking
  # ie class_pred(factor(c(1,1,2)), which = 3)
  constructor(x_data, levels = seq_along(labs), labels = labs)
}

# ordered -> class_pred, assume no equivocal values

#' @method vec_cast.class_pred ordered
#' @export
vec_cast.class_pred.ordered <- function(x, to) {
  vec_cast.class_pred.factor(x, to)
}

# # fully handled by the vec_cast.factor.class_pred method
# # no vec_cast.ordered generic will be implemented, see vctrs#96
# # class_pred -> ordered, equivocals become NAs
# vec_cast.ordered.class_pred <- function(x, to) {
#   as.ordered(vec_cast.factor.class_pred(x, to))
# }

# character -> class_pred, assume no equivocal values

#' @method vec_cast.class_pred factor
#' @export
vec_cast.class_pred.character <- function(x, to) {
  class_pred(factor(x), integer())
}

# class_pred -> character, equivocals become NA

#' @export
#' @importFrom vctrs vec_data
#' @importFrom vctrs warn_lossy_cast
vec_cast.character.class_pred <- function(x, to) {

  # similar implementation as vec_cast.factor.class_pred()
  # but with different lossy cast message. ? -> NA so we want to be noisy
  if(any_equivocal(x)) {
    warn_lossy_cast(x, to, locations = which_equivocal(x))
  }

  x_data <- vec_data(x)
  labs <- attr(x, "labels")

  x_data[is_equivocal(x)] <- NA_integer_

  as.character(factor(x_data, levels = seq_along(labs), labels = labs))

  # # I want to do this, but can't currently cast factor -> character?
  # vec_cast(vec_cast(x, factor()), character())
}

# ------------------------------------------------------------------------------
# Coercion

# -----------------------
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

# -----------------------
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

# # class_pred <-> factor
# # new_factor() is not currently exported!!!?
# vec_type2.class_pred.factor <- function(x, y) new_factor(levels = union_labels_levels(y, x))
# vec_type2.factor.class_pred <- function(x, y) new_factor(levels = union_labels_levels(x, y))
#
# union_labels_levels <- function(x, y) {
#   x_labels <- attr(x, "labels")
#   y_labels <- attr(y, "levels")
#
#   sort(union(x_labels, y_labels))
# }
