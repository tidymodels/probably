cal_pkg_check <- function(pkgs = NULL) {

  not_installed <- pkgs %>%
    purrr::map(
      ~{
        fp <- find.package(.x, quiet = TRUE)
        if(length(fp)) {
          NULL
        } else {
          .x
        }
      }
    ) %>%
    purrr::keep(~ !is.null(.x)) %>%
    as.character()

  if(length(not_installed)) {
    n_pkgs <- length(not_installed)

    pkg_str <- paste0(not_installed, collapse = ", ")
    install_cmd <- paste0("install.packages(", pkg_str ,")")

    cli::cli_abort(paste(
      "{n_pkgs} package{?s} ({.pkg {not_installed}}) {?is/are} needed ",
      "for this calibration but {?is/are} not installed.",
      "To install run: {.run {install_cmd}}"
      ))
  }
  invisible()
}

#' @importFrom generics required_pkgs
#' @keywords internal
#' @export
generics::required_pkgs

#' S3 methods to track which additional packages are needed for specific
#' calibrations
#' @param x A calibration object
#' @inheritParams generics::required_pkgs
#' @export
required_pkgs.cal_object <- function(x, ...) {
  NULL
}
