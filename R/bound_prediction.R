#' Truncate a numeric prediction column
#'
#' For user-defined lower and/or upper bound, ensure that the values in the
#' `.pred` column are coerced to these bounds.
#'
#' @param x A data frame that contains a numeric column named `.pred`.
#' @param lower,upper Single numerics (or `NA`) that define constrains on `.pred`.
#' @return `x` with potentially adjusted values.
#' @examples
#' data(solubility_test, package = "yardstick")
#'
#' names(solubility_test) <- c("solubility", ".pred")
#'
#' bound_prediction(solubility_test, lower = -1)
#' @export
bound_prediction <- function(x, lower = -Inf, upper = Inf) {
  if (!any(names(x) == ".pred")) {
    cli::cli_abort("The argument {.arg x} should have a column named {.code .pred}")
  }
  if (!is.numeric(x$.pred)) {
    cli::cli_abort("Column {.code .pred} should be numeric.")
  }

  if (is.numeric(lower) & !is.na(lower)) {
    x$.pred <- ifelse(x$.pred < lower, lower, x$.pred)
  }

  if (is.numeric(upper) & !is.na(upper)) {
    x$.pred <- ifelse(x$.pred > upper, upper, x$.pred)
  }
  x
}

