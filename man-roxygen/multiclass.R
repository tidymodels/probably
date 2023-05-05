#' @section Multiclass Extension:
#'
#' This method is designed to work with two classes. For multiclass, it creates
#' a set of "one versus all" calibrations for each class. After they are
#' applied to the data, the probability estimates are re-normalized to add to
#' one. This final step might compromise the calibration.
#'
