cal_pkg_check <- function(pkgs = NULL) {
  installed <- purrr::map_lgl(pkgs, rlang::is_installed)

  not_installed <- pkgs[!installed]

  if (length(not_installed)) {
    n_pkgs <- length(not_installed)

    pkg_str <- paste0(not_installed, collapse = ", ")
    install_cmd <- paste0("install.packages(", pkg_str, ")")

    cli::cli_abort(
      c(
        "{n_pkgs} package{?s} ({.pkg {not_installed}}) {?is/are} needed for
         this calibration but {?is/are} not installed.",
        "i" = "To install run: {.run {install_cmd}}"
      )
    )
  }
  invisible()
}

#' S3 methods to track which additional packages are needed for specific
#' calibrations
#' @param x A calibration object
#' @inheritParams generics::required_pkgs
#' @export
required_pkgs.cal_object <- function(x, ...) {
  c("probably")
}
