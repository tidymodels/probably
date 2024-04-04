#' Truncate a numeric prediction column
#'
#' For user-defined `lower_limit` and/or `upper_limit` bound, ensure that the values in the
#' `.pred` column are coerced to these bounds.
#'
#' @param x A data frame that contains a numeric column named `.pred`.
#' @param lower_limit,upper_limit Single numerics (or `NA`) that define
#' constraints on `.pred`.
#' @param call The call to be displayed in warnings or errors.
#' @return `x` with potentially adjusted values.
#' @examples
#' data(solubility_test, package = "yardstick")
#'
#' names(solubility_test) <- c("solubility", ".pred")
#'
#' bound_prediction(solubility_test, lower_limit = -1)
#' @export
bound_prediction <- function(x, lower_limit = -Inf, upper_limit = Inf,
                             call = rlang::current_env()) {
  check_data_frame(x, call = call)

  if (!any(names(x) == ".pred")) {
    cli::cli_abort("The argument {.arg x} should have a column named {.code .pred}.",
                   call = call)
  }
  if (!is.numeric(x$.pred)) {
    cli::cli_abort("Column {.code .pred} should be numeric.", call = call)
  }

  check_number_decimal(lower_limit, allow_na = TRUE, call = call)
  check_number_decimal(upper_limit, allow_na = TRUE, call = call)

  if (!is.na(lower_limit)) {
    x$.pred <- ifelse(x$.pred < lower_limit, lower_limit, x$.pred)
  }

  if (!is.na(upper_limit)) {
    x$.pred <- ifelse(x$.pred > upper_limit, upper_limit, x$.pred)
  }
  x
}

