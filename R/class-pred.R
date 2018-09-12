
#' @importFrom vctrs new_vctr
new_class_pred <- function(x, labels, equivocal_label, ..., subclass = NULL) {

  stopifnot(is.integer(x))
  stopifnot(is.character(labels))
  stopifnot(is.character(equivocal_label))

  new_vctr(
    .data = x,
    labels = labels,
    equivocal_label = equivocal_label,
    ...,
    class = c(subclass, "class_pred", "factor")
  )

}

#' @export
#' @importFrom vctrs vec_cast
#' @importFrom rlang abort
class_pred <- function(x, which = NULL, equivocal_label = "?") {

  if(!is.factor(x)) {
    abort("`x` must be a factor.")
  }

  if(is.null(which)) {
    which <- integer(0)
  }

  if(!is.numeric(which)) {
    abort("`which` must be a numeric.")
  }

  if(!is.character(equivocal_label)) {
    abort("`equivocal_label` must be a character.")
  }

  if(! (length(equivocal_label) == 1) ) {
    abort("`equivocal_label` must be a character.")
  }

  which <- vec_cast(which, integer(0))

  labs <- levels(x)

  x_int <- rlang::as_integer(unclass(x))

  # declare as equivocal
  x_int[which] <- 0L

  new_class_pred(
    x = x_int,
    labels = labs,
    equivocal_label = equivocal_label
  )
}

#' @export
format.class_pred <- function(x) {

  x <- unclass(x)

  x <- x + 1L

  attr(x, "levels") <- c(attr(x, "equivocal_label"), attr(x, "labels"))

  class(x) <- "factor"

  format(x)

  invisible(x)
}

#' @export
`[.class_pred` <- function(x, i, ...) {

  x_new <- rlang::as_integer(unclass(NextMethod()))

  new_class_pred(
    x = x_new,
    labels = attr(x, "labels"),
    equivocal_label = attr(x, "equivocal_label")
  )

}

print.class_pred <- function(x) {
  attr(x, "levels") <- c(attr(x, "equivocal_label"), attr(x, "labels"))
  print.factor(x)
}

as.character.class_pred <- function(x) {

  x <- unclass(x)

  x <- x + 1L

  attr(x, "levels") <- c(attr(x, "equivocal_label"), attr(x, "labels"))

  class(x) <- "factor"

  as.character.factor(x)
}

which_equivocal <- function(x) {
  UseMethod("which_equivocal")
}

which_equivocal.class_pred <- function(x) {
  unclass(x) == 0
}
