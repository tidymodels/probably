#' @importFrom generics fit
#' @export
generics::fit

#' @importFrom generics augment
#' @export
generics::augment

#' @importFrom generics required_pkgs
#' @export
generics::required_pkgs

#' @importFrom tune collect_metrics
#' @export
tune::collect_metrics

#' @importFrom tune collect_predictions
#' @export
tune::collect_predictions

# from tune
# nocov start

is_cran_check <- function () {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

#nocov end
