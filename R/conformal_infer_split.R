#' Prediction intervals via split conformal inference
#'
#' Nonparametric prediction intervals can be computed for fitted regression
#' workflow objects using the split conformal inference method described by
#' Lei _et al_ (2018).
#' @param object A fitted [workflows::workflow()] object.
#' @param ... Not currently used.
#' @param cal_data A data frame with the _original predictor and outcome data_
#' used to produce predictions (and residuals). If the workflow used a recipe,
#' this should be the data that were inputs to the recipe (and not the product
#' of a recipe).
#' @param ... Not currently used.
#' @return An object of class `"int_conformal_split"` containing the
#' information to create intervals (which includes `object`).
#' The `predict()` method is used to produce the intervals.
#' @details
#' This function implements what is usually called "split conformal inference"
#' (see Algorithm 1 in Lei _et al_ (2018)).
#'
#' This function prepares the statistics for the interval computations. The
#' [predict()] method computes the intervals for new data and the signficance
#' level is specified there.
#'
#' `cal_data` should be large enough to get a good estimates of a extreme
#' quantile (e.g., the 95th for 95% interval) and should not include rows that
#' were in the original training set.
#' @seealso [predict.int_conformal_split()]
#' @references
#' Lei, Jing, et al. "Distribution-free predictive inference for regression."
#' _Journal of the American Statistical Association_ 113.523 (2018): 1094-1111.
#' @examplesIf !probably:::is_cran_check()
#' library(workflows)
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(tune)
#' library(modeldata)
#'
#' set.seed(2)
#' sim_train <- sim_regression(500)
#' sim_cal <- sim_regression(200)
#' sim_new <- sim_regression(5) %>% select(-outcome)
#'
#' # We'll use a neural network model
#' mlp_spec <-
#'   mlp(hidden_units = 5, penalty = 0.01) %>%
#'   set_mode("regression")
#'
#' mlp_wflow <-
#'   workflow() %>%
#'   add_model(mlp_spec) %>%
#'   add_formula(outcome ~ .)
#'
#' mlp_fit <- fit(mlp_wflow, data = sim_train)
#'
#' mlp_int <- int_conformal_split(mlp_fit, sim_cal)
#' mlp_int
#'
#' predict(mlp_int, sim_new, level = 0.90)
#' @export
int_conformal_split <- function(object, ...) {
  UseMethod("int_conformal_split")
}

#' @export
#' @rdname int_conformal_split
int_conformal_split.default <- function(object, ...) {
  rlang::abort("No known 'int_conformal_split' methods for this type of object.")
}

#' @export
#' @rdname int_conformal_split
int_conformal_split.workflow <- function(object, cal_data, ...) {
  rlang::check_dots_empty()
  check_data_all(cal_data, object)

  y_name <- names(hardhat::extract_mold(object)$outcomes)
  cal_pred <- generics::augment(object, cal_data)
  cal_pred$.resid <- cal_pred[[y_name]] - cal_pred$.pred
  res <- list(resid = sort(abs(cal_pred$.resid)), wflow = object, n = nrow(cal_pred))
  class(res) <- c("conformal_reg_split", "int_conformal_split")
  res
}

#' @export
print.int_conformal_split <- function(x, ...) {
  cat("Split Conformal inference\n")

  cat("preprocessor:", .get_pre_type(x$wflow), "\n")
  cat("model:", .get_fit_type(x$wflow), "\n")
  cat("calibration set size:", format(x$n, big.mark = ","), "\n\n")

  cat("Use `predict(object, new_data, level)` to compute prediction intervals\n")
  invisible(x)
}

#' @export
#' @rdname predict.int_conformal_full
predict.int_conformal_split <- function(object, new_data, level = 0.95, ...) {
  check_data(new_data, object$wflow)
  rlang::check_dots_empty()

  new_pred <- predict(object$wflow, new_data)

  alpha <- 1 - level
  q_ind <- ceiling(level * (object$n + 1))
  q_val <- object$resid[q_ind]

  new_pred$.pred_lower <- new_pred$.pred - q_val
  new_pred$.pred_upper <- new_pred$.pred + q_val
  new_pred
}

check_data_all <- function(.data, wflow) {
  mold <- hardhat::extract_mold(wflow)
  ptypes <- mold$blueprint$ptypes
  ptypes <- dplyr::bind_cols(ptypes$predictors, ptypes$outcomes)
  hardhat::shrink(.data, ptypes)
  invisible(NULL)
}
