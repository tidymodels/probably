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
# R 3.1 imports

# Required R 3.1 imports, otherwise it can't find them
# through the double dispatch
#' @importFrom vctrs vec_cast.factor
#' @importFrom vctrs vec_cast.integer
NULL

# ------------------------------------------------------------------------------
# Printing

# Abbreviation used in tibbles and str() (through that, rstudio)

#' @export
#' @importFrom vctrs vec_ptype_abbr
vec_ptype_abbr.class_pred <- function(x, ...) {
  "clss_prd"
}

#' @export
#' @importFrom vctrs obj_print_header
obj_print_header.class_pred <- function(x, ...) {
  # no header
  invisible(x)
}

#' @export
#' @importFrom vctrs obj_print_data
obj_print_data.class_pred <- function(x, ...) {
  cat_class_pred(x)
  invisible(x)
}

#' @export
#' @importFrom vctrs obj_print_footer
obj_print_footer.class_pred <- function(x, ...) {
  cat_levels(x)
  cat_reportable(x)
  invisible(x)
}

# ------------------------------------------------------------------------------
# Casting

# -----------------------
# Required casts

#' Cast a `class_pred` vector to a specified type
#'
#' @inheritParams vctrs::vec_cast
#'
#' @export
#' @method vec_cast class_pred
#' @export vec_cast.class_pred
#' @importFrom vctrs vec_cast
vec_cast.class_pred <- function(x, to, ...) UseMethod("vec_cast.class_pred")

#' @method vec_cast.class_pred default
#' @export
#' @importFrom vctrs vec_default_cast
vec_cast.class_pred.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @method vec_cast.class_pred class_pred
#' @export
vec_cast.class_pred.class_pred <- function(x, to, ...) {

  # first go class_pred -> factor
  # then recast as class_pred with correct attributes

  class_pred(
    x = factorish_to_factor(x, to),
    which = which_equivocal(x),
    equivocal = get_equivocal_label(to)
  )
}

# -----------------------
# Custom casts

# factor -> class_pred, assume no equivocal values

#' @method vec_cast.class_pred factor
#' @export
vec_cast.class_pred.factor <- function(x, to, ...) {
  class_pred(
    x = factorish_to_factor(x, to),
    which = integer(),
    equivocal = get_equivocal_label(to)
  )
}

# class_pred -> factor, equivocals become NAs

#' @export
#' @importFrom vctrs vec_data
#' @importFrom vctrs maybe_lossy_cast
vec_cast.factor.class_pred <- function(x, to, ...) {
  factorish_to_factor(x, to)
}

# ordered -> class_pred

#' @method vec_cast.class_pred ordered
#' @export
vec_cast.class_pred.ordered <- vec_cast.class_pred.factor

# # fully handled by the vec_cast.factor.class_pred method
# # no vec_cast.ordered generic will be implemented, see vctrs#96
# vec_cast.ordered.class_pred <- function(x, to) {
#   as.ordered(vec_cast.factor.class_pred(x, to))
# }

# character -> class_pred

#' @method vec_cast.class_pred character
#' @export
vec_cast.class_pred.character <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  # first cast character -> factor
  # then add class pred attributes

  rethrow_lossy_cast <- function(e) {
    # have to manually recover `lossy` because vctrs 0.2.0 doesn't pass it through (vctrs#483)
    lossy <- !(x %in% levels(to) | is.na(x))
    maybe_lossy_cast(e$result, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
  }

  out <- tryCatch(
    expr = vec_cast.factor(
      x = x,
      to = factor(levels = levels(to), ordered = is_ordered(to))
    ),
    vctrs_error_cast_lossy = rethrow_lossy_cast
  )

  class_pred(
    x = out,
    equivocal = get_equivocal_label(to)
  )
}

# class_pred -> character, equivocals become NA

#' @export
#' @importFrom vctrs vec_data
vec_cast.character.class_pred <- function(x, to, ...) {
  x_data <- vec_data(x)
  x_data[is_equivocal(x)] <- NA_integer_
  lvls <- levels(x)
  lvls[x_data]
}

# `to` could have different levels
# when `to` does not have all the levels in `x`, the missing levels
# in `to` are converted to NA in `x` and a lossy cast error is maybe issued.
factorish_to_factor <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (length(levels(to)) == 0L) {
    factor(x = as.character(x), levels = levels(x), ordered = is_ordered(to))
  }
  else {
    chr_x <- as.character(x)
    lvls <- levels(to)

    lossy <- ! (chr_x %in% levels(to) | is.na(chr_x))

    locations <- which(lossy)
    chr_x[locations] <- NA_integer_

    out <- factor(x = chr_x, levels = lvls, ordered = is_ordered(to))

    maybe_lossy_cast(
      result = out,
      x = x,
      to = to,
      lossy = lossy,
      locations = locations,
      x_arg = x_arg,
      to_arg = to_arg
    )
  }
}


# ------------------------------------------------------------------------------
# Coercion

# -----------------------
# Required coercion

#' Find the common type for a `class_pred` and another object
#'
#' @inheritParams vctrs::vec_ptype2
#'
#' @export
#' @method vec_ptype2 class_pred
#' @export vec_ptype2.class_pred
#' @importFrom vctrs vec_ptype2
vec_ptype2.class_pred <- function(x, y, ...) {
  UseMethod("vec_ptype2.class_pred", y)
}

#' @method vec_ptype2.class_pred default
#' @export
#' @importFrom vctrs vec_default_ptype2
vec_ptype2.class_pred.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# -----------------------
# Custom coercion

# class_pred + class_pred = class_pred with unioned labels
# it is ordered if either are ordered
# the new eq label always comes from x

#' @method vec_ptype2.class_pred class_pred
#' @export
vec_ptype2.class_pred.class_pred <- function(x, y, ...) {
  new_class_pred(
    integer(),
    labels = union_labels(x, y),
    ordered = union_ordered(x, y),
    equivocal = get_equivocal_label(x)
  )
}

#' @method vec_ptype2.class_pred factor
#' @export
vec_ptype2.class_pred.factor <- function(x, y, ...) {
  new_class_pred(
    x = integer(),
    labels = union_labels(x, y),
    ordered = is_ordered_class_pred(x),
    equivocal = get_equivocal_label(x)
  )
}

#' @export
#' @importFrom vctrs vec_ptype2.factor
vec_ptype2.factor.class_pred <- function(x, y, ...) {
  new_class_pred(
    x = integer(),
    labels = union_labels(x, y),
    ordered = is_ordered_class_pred(y),
    equivocal = get_equivocal_label(y)
  )
}

#' @method vec_ptype2.class_pred character
#' @export
vec_ptype2.class_pred.character <- function(x, y, ...) {
  character()
}

#' @export
#' @importFrom vctrs vec_ptype2.character
vec_ptype2.character.class_pred <- function(x, y, ...) {
  character()
}

union_labels <- function(x, y) {
  x_labels <- levels(x)
  y_labels <- levels(y)

  union(x_labels, y_labels)
}

union_ordered <- function(x, y) {
  is_ordered_class_pred(x) | is_ordered_class_pred(y)
}

# ------------------------------------------------------------------------------
# Comparison and equality

#' Equality for `class_pred`
#'
#' `class_pred` objects are converted to integer before equality checks
#' are done.
#'
#' @export
#' @importFrom vctrs vec_proxy_equal
#' @keywords internal
vec_proxy_equal.class_pred <- function(x, ...) {
  # allows you to compare two class_pred objects robustly
  # converting to character would confuse NA with equivocal
  vec_data(x)
}

#' @export
#' @importFrom vctrs vec_proxy_compare
vec_proxy_compare.class_pred <- function(x, ...) {
  abort("Comparisons with `class_pred` objects are not meaningful.")
}
