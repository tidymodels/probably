#' Prediction intervals via conformal inference
#'
#' Nonparametric prediction intervals can be computed for fitted workflow
#' objects using the conformal inference method described by Lei _at al_ (2018).
#'
#' @param object A fitted [workflows::workflow()] object.
#' @param new_data A data frame of predictors.
#' @param level The confidence level for the intervals.
#' @param control A control object from [control_conformal_infer()] with the
#' numeric minutiae.
#' @param ... Not currently used.
#' @param train_data A data frame with the _original predictor data_ used to
#' create the fitted workflow (predictors and outcomes). If the workflow does
#' not contain these values, pass them here. If the workflow used a recipe, this
#' should be the data that were inputs to the recipe (and not the product of a
#' recipe). It should be a named argument.
#' @return A tibble with columns `.pred_lower`, `.pred`, and `.pred_upper`. If
#' the search for the prediction bound fails, a missing value is used.
#'
#' @details
#' This function implements what is usually called "full comformal inference"
#' (see Algorithm 1 in Lei _et al_ (2018)) since it uses the entire training
#' set to compute the intervals.
#'
#' For a given prediction, different "trial" values of the outcome are evaluated
#' but augmenting the training set with the sample being predicted (with the
#' trial value as the outcome). From this model, the residual associated with
#' the trial value is compared to a quantile of the distribution of the other
#' residuals. Usually the absolute values of the residuals are used. Once the
#' residual of a trial value exceeds the distributional quantile, the value is
#' one of the bounds.
#'
#' The literature proposed using a grid search of trial values to find the two
#' points that correspond to the prediction intervals. This can be done via
#' an option in [control_conformal_infer()]. However, the default approach uses
#' two different one dimensional searches on either side of the predicted value
#' to find values that correspond to the prediction intervals.
#'
#' For medium to large data sets, the iterative search method is likely to
#' generate slightly smaller intervals. For small training sets, grid search
#' is more likely to have somewhat smaller intervals (and will be more stable).
#' Otherwise, the iterative search method is more precise and several folds
#' faster.
#'
#' To determine a range of possible values of the intervals, used by both methods,
#' the initial set of training set residuals are modeled using a Gamma generalized
#' linear model with a log link (see the reference by Aitkin below). For a new
#' sample, the absolute size of the residual is estimated and a multiple of
#' this value is computed as an initial guess of the search boundaries.
#'
#' @includeRmd man/rmd/parallel_intervals.Rmd details
#'
#' @references
#' Jing Lei, Max G'Sell, Alessandro Rinaldo, Ryan J. Tibshirani and Larry
#' Wasserman (2018) Distribution-Free Predictive Inference for Regression,
#' _Journal of the American Statistical Association_, 113:523, 1094-1111
#'
#' Murray Aitkin, Modelling Variance Heterogeneity in Normal Regression Using
#' GLIM, _Journal of the Royal Statistical Society Series C: Applied Statistics_,
#' Volume 36, Issue 3, November 1987, Pages 332–339.
#' @export
int_conformal_infer <- function(object, ...) {
  UseMethod("int_conformal_infer")
}

#' @export
#' @rdname int_conformal_infer
int_conformal_infer.default <- function(object, ...) {
  rlang::abort("No known 'int_conformal_infer' methods for this type of object.")
}

#' @export
#' @rdname int_conformal_infer
int_conformal_infer.workflow <-
  function(object, new_data, ..., train_data, level = 0.95,
           control = control_conformal_infer()) {

    check_workflow(object)

    # --------------------------------------------------------------------------
    # check req packages

    pkgs <- required_pkgs(object)
    pkgs <- unique(c(pkgs, "workflows", "parsnip", "probably", control$required_pkgs))
    rlang::check_installed(pkgs)
    control$required_pkgs <- pkgs

    # --------------------------------------------------------------------------

    if (is.null(train_data)) {
      train_data <- data_from_wflow(object)
    } else {
      new_data <- hardhat::scream(new_data, object$blueprint$ptypes$predictors)
    }

    y_name <- get_outcome_name(object)

    # --------------------------------------------------------------------------
    # We need to set the potential range that encompassing the _possible_ values
    # of the prediction interval. This is done on a sample-by-sample basis using
    # a variance model on the training set residuals.

    new_pred <- setup_new_data(object, new_data, train_data, control$var_multiplier)

    # ------------------------------------------------------------------------------
    # split new_data for row-wise processing

    new_nest <-
      dplyr::bind_cols(new_data, new_pred) %>%
      dplyr::mutate(.row = dplyr::row_number()) %>%
      dplyr::group_by(.row) %>%
      tidyr::nest()

    # ------------------------------------------------------------------------------
    # compute intervals

    if (control$method == "grid") {
      res <- grid_all(new_nest$data, object, train_data, level, control)
    } else {
      res <- optimize_all(new_nest$data, object, train_data, level, control)
    }
    if(control$progress) {
      cat("\n")
    }
    res
  }

# ------------------------------------------------------------------------------

data_from_wflow <- function(x) {
  dplyr::bind_cols(x$pre$mold$outcomes, x$pre$mold$predictors)
}

check_train_data <- function(.data, wflow) {
  # TODO use `wflow$pre$mold$blueprint$ptypes$predictors` and outcome to check for columns
  invisible(NULL)
}

get_outcome_name <- function(x) {
  names(x$pre$mold$blueprint$ptypes$outcomes)
}

get_mode <- function(x) {
  x %>%
    hardhat::extract_fit_parsnip() %>%
    purrr::pluck("spec") %>%
    purrr::pluck("mode")
}

check_workflow <- function(x) {
  if (!workflows::is_trained_workflow(x)) {
    rlang::abort("'object' should be a fitted workflow object.")
  }
  if (get_mode(x) != "regression") {
    rlang::abort("'object' should be a regression model.")
  }
}

# ------------------------------------------------------------------------------
# Get a rough estimate of the local variance for each new sample being
# predicted. Aitkin (1987) gives a framework where, assuming normality of
# residuals, the squared residuals can be modeled using Gamma errors and the
# log link.
#
# This most likely underestimates the local variance so use a multiplier on the
# variance prediction to make the possible range of the bounds really wide.

setup_new_data <- function(object, new_data, train_data, multiplier = 10) {

  y_name <- get_outcome_name(object)

  new_pred <- predict(object, new_data)

  train_res <- predict(object, train_data)
  train_res$resid <- train_data[[y_name]] - train_res$.pred
  train_res$sq <- train_res$resid^2

  # ------------------------------------------------------------------------------
  # Get ballpark estimate of range of trial values for new data

  # Use squared residuals as outcome in a gamma model to predict the standard
  # deviation at a given prediction.
  var_mod <-
    mgcv::gam(sq ~ s(.pred),
              data = train_res,
              family = stats::Gamma(link = "log"))

  # previously:
  # var_mod <-
  #   stats::glm(sq ~ splines::ns(.pred, df = 5),
  #              data = train_res,
  #              family = stats::Gamma(link = "log"))
  #
  # Get prediction on squared errors
  var_pred <- as.vector(predict(var_mod, new_pred, type = "response"))
  # convert back to original units
  var_pred <- sqrt(var_pred)
  # Add a buffer
  new_pred$.bound <- 10 * var_pred
  new_pred

}

# ------------------------------------------------------------------------------
# helpers for trial values

# ------------------------------------------------------------------------------
# For a trial value of the bounds, fit the model and return statistics
# combine train_data and new_data

trial_fit <- function(trial, trial_data, wflow, level) {
  y_name <- get_outcome_name(wflow)

  # trial_data is training set with one extra row for the sample being inferred
  # Last value of the outcome vector is NA; add our trial value there
  trial_data[[y_name]][nrow(trial_data)] <- trial
  tmp_fit <- try(fit(wflow, trial_data), silent = TRUE)
  if (inherits(tmp_fit, "try-error")) {
    return(
      dplyr::tibble(quantile = NA_real_, trial = trial, .abs_resid = NA_real_)
    )
  }

  # Compute the abs residuals then more statistics
  tmp_res <-
    augment(tmp_fit, trial_data) |>
    dplyr::mutate(
      .abs_resid = abs(!!rlang::sym(y_name) - .pred)
    )
  # Quantiles of original training set (abs) residuals
  quant_val <- stats::quantile(tmp_res$.abs_resid[-1], prob = level)

  res <-
    dplyr::tibble(
      quantile = unname(quant_val),
      trial = trial,
      .abs_resid = tmp_res$.abs_resid[nrow(trial_data)]
    )
  res$difference <- res$.abs_resid - res$quantile
  res
}

# ------------------------------------------------------------------------------

grid_all <- function(new_data, model, train_data, level, ctrl) {

  # purrr::map_dfr(
  furrr::future_map_dfr(
    new_data,
    grid_one,
    model,
    train_data,
    level,
    ctrl,
    .progress = ctrl$progress,
    .options = furrr::furrr_options(seed = ctrl$seed, stdout = FALSE)
  )
}

grid_one <- function(new_data, model, train_data, level, ctrl) {
  y_name <- get_outcome_name(model)

  pkg_inst <- lapply(ctrl$required_pkgs, rlang::is_installed)
  pred_val <- new_data$.pred
  bound <- new_data$.bound
  new_data <- dplyr::select(new_data, -.pred, -.bound)
  new_data[[y_name]] <- NA_real_
  trial_data <- dplyr::bind_rows(train_data, new_data)

  trial_vals <- seq(pred_val - bound, pred_val + bound, length.out = ctrl$trial_points)

  res <-
    purrr::map_dfr(
      trial_vals,
      ~ trial_fit(.x,
                  trial_data = trial_data,
                  wflow = model,
                  level = level)
    )

  # if (!is.null(ctrl$testing)) {
  #   file_nm <- format(Sys.time(), "_%Y_%m_%d")
  #   file_nm <- paste("~/tmp/conf", signif(pred_val, 4), file_nm, sample.int(100, 1), ".RData", sep = "_")
  #   grid_res <- res
  #   grid_res$.pred <- pred_val
  #   save(grid_res, file = file_nm)
  # }

  compute_bound(res, pred_val)
}

compute_bound <- function(x, predicted) {
  x <- x[stats::complete.cases(x),]
  if (all(x$difference < 0)) {
    rlang::warn("Could not determine bounds.")
    res <-
      dplyr::tibble(.pred_lower = NA_real_,
                    .pred = predicted,
                    .pred_upper = NA_real_)
    return(res)
  }

  upper <- x[x$trial >= predicted & x$difference >= 0,]
  if (nrow(upper) > 0) {
    upper <- min(upper$trial)
  } else {
    upper <- NA_real_
  }
  lower <- x[x$trial <= predicted & x$difference >= 0,]
  if (nrow(lower) > 0) {
    lower <- max(lower$trial)
  } else {
    lower <- NA_real_
  }
  dplyr::tibble(.pred_lower = lower, .pred = predicted, .pred_upper = upper)
}

# ------------------------------------------------------------------------------

optimize_all <- function(new_data, model, train_data, level, ctrl) {
  # purrr::map_dfr(
  furrr::future_map_dfr(
    new_data,
    optimize_one,
    model = model,
    train_data = train_data,
    level = level,
    ctrl,
    .progress = ctrl$progress,
    .options = furrr::furrr_options(seed = ctrl$seed, stdout = FALSE)
  )
}

get_diff <- function(val, ...) {
  res <- trial_fit(val, ...)
  # res$.when <- date()
  # file_nm <- paste0("~/tmp/opt_", sample.int(10^5, 1), ".RData")
  # save(res, file = file_nm)
  # print(res)
  res$difference
}

optimize_one <- function(new_data, model, train_data, level, ctrl) {
  y_name <- get_outcome_name(model)

  pkg_inst <- lapply(ctrl$required_pkgs, rlang::is_installed)
  pred_val <- new_data$.pred
  bound <- new_data$.bound
  new_data <- dplyr::select(new_data, -.pred, -.bound)
  new_data[[y_name]] <- NA_real_
  trial_data <- dplyr::bind_rows(train_data, new_data)

  upper <-
    try(
      stats::uniroot(get_diff, c(pred_val, pred_val + bound),
                     maxiter = ctrl$max_iter, tol = ctrl$tolerance,
                     extendInt = "upX",
                     trial_data, model, level),
      silent = TRUE)

  lower <-
    try(
      stats::uniroot(get_diff, c(pred_val - bound, pred_val),
                     maxiter = ctrl$max_iter, tol = ctrl$tolerance,
                     extendInt = "downX",
                     trial_data, model, level),
      silent = TRUE)

  dplyr::tibble(
    .pred_lower = get_root(lower, ctrl),
    .pred = pred_val,
    .pred_upper = get_root(upper, ctrl)
  )
}

get_root <- function(x, ctrl) {
  if (inherits(x, "try-error")) {
    rlang::warn("Could not finish the search process due to errors.")
    return(NA_real_)
  }
  if (x$iter == ctrl$max_iter) {
    rlang::warn("Search did not converge.")
    return(NA_real_)
  }
  return(x$root)
}

# ------------------------------------------------------------------------------

#' Controlling the numeric details for conformal inference
#'
#'
#' @param method The method for computing the intervals. The options are
#' `'search'` (using) [stats::uniroot()], and `'grid'`.
#' @param trial_points When `method = "grid"`, how many points should be
#' evaluated?
#' @param var_multiplier A multiplier for the variance model that determines the
#' possible range of the bounds.
#' @param max_iter When `method = "search"`, the maximum number of iterations.
#' @param tolerance Tolerance value passed to [all.equal()] to determine
#' convergence during the search computations.
#' @param progress Should a progress bar be used to track execution?
#' @param required_pkgs An optional character string for which packages are
#' required.
#' @param seed A single integer used to control randomness when models are
#' (re)fit.
#' @return A list object with the options given by the user.
#' @export
control_conformal_infer <-
  function(method = "search", trial_points = 100, var_multiplier = 10,
           max_iter = 100, tolerance = .Machine$double.eps^0.25, progress = FALSE,
           required_pkgs = character(0), seed = NULL) {
    method <- rlang::arg_match0(method, c("search", "grid"))
    if (is.null(seed)) {
      seed <- sample.int(10^5, 1)
    } else {
      seed <- as.integer(seed)[1]
    }
    list(
      method = method,
      trial_points = trial_points,
      var_multiplier = var_multiplier,
      max_iter = max_iter,
      tolerance = tolerance,
      progress = progress,
      seed = seed
    )
  }