# ------------------------------------------------------------------------
# vec_type2()

vec_type2.class_pred <- function(x, y) {
  UseMethod("vec_type2.class_pred", y)
}

vec_type2.class_pred.unknown <- function(x, y) {
  x[0L]
}

vec_type2.class_pred.default <- function(x, y) {
  vctrs::stop_incompatible_type(x, y)
}

# Common type: class_pred <-> factor = factor
vec_type2.class_pred.factor <- function(x, y) factor()
vec_type2.factor.class_pred <- function(x, y) factor()

# Common type: class_pred <-> character = character
vec_type2.character.class_pred <- function(x, y) character()
vec_type2.class_pred.character <- function(x, y) character()

union_labels <- function(x, y) {
  x_labels <- attr(x, "labels")
  y_labels <- attr(y, "labels")

  sort(union(x_labels, y_labels))
}

vec_type2.class_pred.class_pred <- function(x, y) {
  new_class_pred(
    x = integer(),
    labels = union_labels(x, y),
    # equivocal label comes from x
    equivocal_label = attr(x, "equivocal_label")
  )
}

# ------------------------------------------------------------------------
# vec_type2()


vec_cast.class_pred <- function(x, to) {
  UseMethod("vec_cast.class_pred")
}

vec_cast.class_pred.unknown <- function(x, to) {
  NULL
}

vec_cast.class_pred.default <- function(x, to) {
  vctrs::stop_incompatible_cast(x, to)
}

# Required list casts
as.list.class_pred <- function(x, ...) {
  lapply(seq_along(x), function(i) x[i])
}

vec_cast.list.class_pred <- function(x, to) as.list(x)
# eh?
vec_cast.class_pred.list <- function(x, to) vctrs::vec_list_cast(x, to)

# Cast to self
vec_cast.class_pred.class_pred <- function(x, to) {
  new_class_pred(as.vector(x), breaks = attr(x, "breaks"))
}


