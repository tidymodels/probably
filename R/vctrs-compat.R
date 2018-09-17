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
  lvls <- attr(x, "labels")
  lvls <- paste0(lvls, collapse = " ")

  cat_lvls <- "Levels: "
  cat_lvls <- paste0(cat_lvls, lvls)
  cat(cat_lvls)

  cat("\n")

  eq_count <- sum(is_equivocal(x))
  cat_eq <- "EQ Count: "
  cat_eq <- paste0(cat_eq, eq_count)
  cat(cat_eq)
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

  warn_lossy_cast(x, to, locations = which_equivocal(x))

  x_data <- vec_data(x)
  labs <- attr(x, "labels")

  x_data[is_equivocal(x)] <- NA_integer_

  factor(x_data, labels = labs)
}

# character -> class_pred, assume no equivocal values

#' @method vec_cast.class_pred factor
#' @export
vec_cast.class_pred.character <- function(x, to) {
  class_pred(factor(x), integer())
}

# class_pred -> character, equivocals become NA
# as.factor() comes for free with this, but the lossy message is displayed
# twice because it calls as.character() twice

#' @export
#' @importFrom vctrs vec_data
#' @importFrom vctrs warn_lossy_cast
vec_cast.character.class_pred <- function(x, to) {

  # same implementation as vec_cast.factor.class_pred()
  # but with different lossy cast message. ? -> NA so we want to be noisy
  if(any_equivocal(x)) {
    warn_lossy_cast(x, to, locations = which_equivocal(x))
  }

  x_data <- vec_data(x)
  labs <- attr(x, "labels")

  x_data[is_equivocal(x)] <- NA_integer_

  as.character(factor(x_data, labels = labs))

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
