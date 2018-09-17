#' @import rlang
#' @importFrom utils globalVariables

# is there a forcats for this?
recode_data <- function(obs, prob, threshold) {
  lvl <- levels(obs)
  if (getOption("yardstick.event_first")) {
    pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  } else {
    pred <- ifelse(prob >= threshold, lvl[2], lvl[1])
  }
  factor(pred, levels = lvl)
}

