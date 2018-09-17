#' @import rlang

## nocov start

dummy <- function() { }

.onLoad <- function(libname, pkgname) {

  pkgenv <- environment(dummy)

  makeActiveBinding(
    "global_equivocal_label",
    function() getOption("probably.equivocal_label", "EQ"),
    pkgenv
  )

}

## nocov end
