#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom yardstick sens spec j_index
#' @importFrom stats binomial median predict qnorm
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".bin", ".is_val", "event_rate", "events", "lower",
  "predicted_midpoint", "total", "upper", ".config"
))
