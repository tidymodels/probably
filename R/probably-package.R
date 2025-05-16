#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom utils head
#' @importFrom yardstick sens spec j_index
#' @importFrom stats binomial median predict qnorm as.stepfun glm isoreg prop.test
## usethis namespace: end
NULL

utils::globalVariables(c(
  ".bin", ".is_val", "event_rate", "events", "lower",
  "predicted_midpoint", "total", "upper", ".config",
  ".adj_estimate", ".rounded", ".pred", ".bound", "pred_val", ".extracts",
  ".x", ".type", ".metrics", "cal_data"
))
