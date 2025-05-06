#' Prediction intervals via conformal inference and quantile regression
#'
#' Nonparametric prediction intervals can be computed for fitted regression
#' workflow objects using the split conformal inference method described by
#' Romano _et al_ (2019). To compute quantiles, this function uses Quantile
#' Random Forests instead of classic quantile regression.
#' @param object A fitted [workflows::workflow()] object.
#' @param ... Not currently used.
#' @param train_data,cal_data Data frames with the _predictor and outcome data_.
#' `train_data` should be the same data used to produce `object` and `cal_data` is
#' used to produce predictions (and residuals). If the workflow used a recipe,
#' these should be the data that were inputs to the recipe (and not the product
#' of a recipe).
#' @param level The confidence level for the intervals.
#' @param ... Options to pass to [quantregForest::quantregForest()] (such as the
#' number of trees).
#' @return An object of class `"int_conformal_quantile"` containing the
#' information to create intervals (which includes `object`).
#' The `predict()` method is used to produce the intervals.
#' @details
#' Note that the significance level should be specified in this function
#' (instead of the `predict()` method).
#'
#' `cal_data` should be large enough to get a good estimates of a extreme
#' quantile (e.g., the 95th for 95% interval) and should not include rows that
#' were in the original training set.
#'
#' Note that the because of the method used to construct the interval, it is
#' possible that the prediction intervals will not include the predicted value.
#' @seealso [predict.int_conformal_quantile()]
#' @references
#' Romano, Yaniv, Evan Patterson, and Emmanuel Candes. "Conformalized quantile
#' regression." _Advances in neural information processing systems_ 32 (2019).
#' @examplesIf !probably:::is_cran_check() & rlang::is_installed(c("modeldata", "parsnip", "quantregForest"))
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
#' sim_new <- sim_regression(5) |> select(-outcome)
#'
#' # We'll use a neural network model
#' mlp_spec <-
#'   mlp(hidden_units = 5, penalty = 0.01) |>
#'   set_mode("regression")
#'
#' mlp_wflow <-
#'   workflow() |>
#'   add_model(mlp_spec) |>
#'   add_formula(outcome ~ .)
#'
#' mlp_fit <- fit(mlp_wflow, data = sim_train)
#'
#' mlp_int <- int_conformal_quantile(mlp_fit, sim_train, sim_cal,
#'   level = 0.90
#' )
#' mlp_int
#'
#' predict(mlp_int, sim_new)
#' @export
int_conformal_quantile <- function(object, ...) {
  UseMethod("int_conformal_quantile")
}


#' @export
#' @rdname int_conformal_quantile
int_conformal_quantile.workflow <-
  function(object, train_data, cal_data, level = 0.95, ...) {
    check_data_all(train_data, object)
    check_data_all(cal_data, object)

    # ------------------------------------------------------------------------------

    y_name <- names(hardhat::extract_mold(object)$outcomes)

    quant_fit <- quant_train(train_data, y_name, ...)
    quant_pred <- quant_predict(quant_fit, cal_data, level)

    # ------------------------------------------------------------------------------

    R_low <- quant_pred$.pred_lower - cal_data[[y_name]]
    R_high <- cal_data[[y_name]] - quant_pred$.pred_upper
    resid <- pmax(R_low, R_high)

    # ------------------------------------------------------------------------------

    res <-
      list(
        resid = sort(resid),
        wflow = object,
        n = nrow(cal_data),
        quant = quant_fit,
        level = level
      )
    class(res) <- c("conformal_reg_quantile", "int_conformal_quantile")
    res
  }

#' @export
print.int_conformal_quantile <- function(x, ...) {
  cat("Split Conformal inference via Quantile Regression\n")

  cat("preprocessor:", .get_pre_type(x$wflow), "\n")
  cat("model:", .get_fit_type(x$wflow), "\n")
  cat("calibration set size:", format(x$n, big.mark = ","), "\n")
  cat("confidence level:", format(x$level, digits = 3), "\n\n")

  cat("Use `predict(object, new_data)` to compute prediction intervals\n")
  invisible(x)
}

#' @export
#' @rdname predict.int_conformal_full
predict.int_conformal_quantile <- function(object, new_data, ...) {
  check_data(new_data, object$wflow)
  rlang::check_dots_empty()

  new_pred <- predict(object$wflow, new_data)
  quant_pred <- quant_predict(object$quant, new_data, object$level)

  alpha <- (1 - object$level)
  q_ind <- ceiling((1 - alpha) * (object$n + 1))
  q_val <- object$resid[q_ind]

  quant_pred$.pred_lower <- quant_pred$.pred_lower - q_val
  quant_pred$.pred_upper <- quant_pred$.pred_upper + q_val
  dplyr::bind_cols(new_pred, quant_pred)
}

quant_train <- function(train_data, y_name, ...) {
  rlang::check_installed("quantregForest", reason = "to compute quantiles")
  cl <- rlang::call2(
    "quantregForest",
    .ns = "quantregForest",
    x = quote(train_data |> select(-dplyr::all_of(y_name))),
    y = quote(train_data[[y_name]]),
    ...
  )
  rlang::eval_tidy(cl)
}

quant_predict <- function(fit, new_data, level) {
  rlang::check_installed("quantregForest", reason = "to compute quantiles")
  alpha <- (1 - level)
  quant_pred <- predict(fit, new_data, what = c(alpha / 2, 1 - (alpha / 2)))
  quant_pred <- dplyr::as_tibble(quant_pred)
  quant_pred <- stats::setNames(quant_pred, c(".pred_lower", ".pred_upper"))
  quant_pred
}
