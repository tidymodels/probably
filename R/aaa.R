#' @import rlang
#' @importFrom utils globalVariables

## nocov start

dummy <- function() { }

.onLoad <- function(libname, pkgname) {

  pkgenv <- environment(dummy)

  makeActiveBinding(
    "probably.equivocal_label",
    function() getOption("probably.equivocal_label", "EQ"),
    pkgenv
  )

}

requireNamespace("yardstick", quietly = TRUE)

## nocov end
