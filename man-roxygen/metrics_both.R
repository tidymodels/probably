#' @section Performance Metrics:
#'
#' By default, the average of the Brier scores (classification calibration) or the
#' root mean squared error (regression) is returned. Any appropriate
#' [yardstick::metric_set()] can be used. The validation function compares the
#' average of the metrics before, and after the calibration.
