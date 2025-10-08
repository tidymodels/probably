#' Butcher methods for conformal inteference intervals
#'
#' These methods allow you to use the butcher package to reduce the size of a
#' conformal inference interval object. After calling `butcher::butcher()` on a
#' conformal inference interval object, the only guarantee is that you will
#' still be able to `predict()` from that conformal inference interval object.
#' Other functions may not work as expected.
#'
#' @param x A conformal inference interval object.
#' @param verbose Should information be printed about how much memory is freed
#'   from butchering?
#' @param ... Extra arguments possibly used by underlying methods.
#'
#' @name inf_conformal-butcher

# int_conformal_full

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_call.int_conformal_full <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_call(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_ctrl.int_conformal_full <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_ctrl(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_data.int_conformal_full <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_data(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_env.int_conformal_full <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_env(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_fitted.int_conformal_full <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_fitted(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# int_conformal_split

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_call.int_conformal_split <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_call(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_ctrl.int_conformal_split <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_ctrl(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_data.int_conformal_split <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_data(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_env.int_conformal_split <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_env(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_fitted.int_conformal_split <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_fitted(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# int_conformal_quantile

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_call.int_conformal_quantile <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_call(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_ctrl.int_conformal_quantile <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_ctrl(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_data.int_conformal_quantile <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_data(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_env.int_conformal_quantile <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_env(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_fitted.int_conformal_quantile <- function(x, verbose = FALSE, ...) {
  x$wflow <- butcher::axe_fitted(x$wflow, verbose = verbose, ...)
  add_butcher_class(x)
}

# int_conformal_cv

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_call.int_conformal_cv <- function(x, verbose = FALSE, ...) {
  x$models <- purrr::map(x$models, butcher::axe_call, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_ctrl.int_conformal_cv <- function(x, verbose = FALSE, ...) {
  x$models <- purrr::map(x$models, butcher::axe_ctrl, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_data.int_conformal_cv <- function(x, verbose = FALSE, ...) {
  x$models <- purrr::map(x$models, butcher::axe_data, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_env.int_conformal_cv <- function(x, verbose = FALSE, ...) {
  x$models <- purrr::map(x$models, butcher::axe_env, verbose = verbose, ...)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname inf_conformal-butcher
axe_fitted.int_conformal_cv <- function(x, verbose = FALSE, ...) {
  x$models <- purrr::map(x$models, butcher::axe_fitted, verbose = verbose, ...)
  add_butcher_class(x)
}

# ------------------------------------------------------------------------------

# butcher:::add_butcher_class
add_butcher_class <- function(x) {
  if (!any(grepl("butcher", class(x)))) {
    class(x) <- append(paste0("butchered_", rev(class(x))[1]), class(x))
  }
  x
}
