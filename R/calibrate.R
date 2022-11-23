
# ------------------------------ Logistic --------------------------------------


#' @export
cal_logistic <- function(.data,
                         truth = NULL,
                         estimate = NULL,
                         event_level = c("first", "second"),
                         ...
                         ) {
  UseMethod("cal_logistic")
}

#' @export
cal_logistic.data.frame <- function(.data,
                                    truth = NULL,
                                    estimate = NULL,
                                    event_level = c("first", "second"),
                                    ...
                                    ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  cal_logistic_impl(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    event_level = event_level,
    model = "glm",
    method = "Logistic",
    additional_class = "cal_logistic"
    )
}

# --------------------- Logistic Spline (GAM)  ---------------------------------

#' @export
cal_logistic_spline <- function(.data,
                                truth = NULL,
                                estimate = NULL,
                                event_level = c("first", "second"),
                                ...
                                ) {
  UseMethod("cal_logistic_spline")
}

#' @export
cal_logistic_spline.data.frame <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           event_level = c("first", "second"),
                                           ...
                                           ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  cal_logistic_impl(
    .data = .data,
    truth = !!truth,
    estimate = !!estimate,
    event_level = event_level,
    model = "logistic_spline",
    method = "Logistic Spline",
    additional_class = "cal_logistic_spline"
  )
}

# -------------------------------- Beta ----------------------------------------

#' @export
cal_beta <- function(.data, truth = NULL, estimate = NULL, ...) {
  UseMethod("cal_beta")
}

#' @export
cal_beta.data.frame <- function(.data, truth = NULL, estimate = NULL, ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_model_impl(.data, !!truth, !!estimate, method = "beta", ... = ...)
    est <- as_cal_estimate(res, !! estimate)
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_beta")
}

# ----------------------------- Isotonic ---------------------------------------

#' @export
cal_isotonic <- function(.data, truth = NULL, estimate = NULL, ...) {
  UseMethod("cal_isotonic")
}

#' @export
cal_isotonic.data.frame <- function(.data, truth = NULL, estimate = NULL, ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_dataframe(.data, !!truth, !!estimate, ... = ...)
    est <- as_cal_estimate(res, !! estimate)
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_isotonic")
}

cal_isoreg_dataframe <- function(.data, truth, estimate, truth_val = NULL,
                                 sampled = FALSE, ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  truth_data <- add_is_val(.data, !! truth, truth_val)

  if(sampled) {
    sort_data <- dplyr::slice_sample(truth_data, prop = 1, replace = TRUE)
  } else {
    sort_data <- dplyr::arrange(truth_data, !! estimate)
  }

  model <- isoreg(
    dplyr::pull(sort_data, !!estimate),
    dplyr::pull(sort_data, .is_val)
  )

  model_stepfun <- as.stepfun(model, ... = ...)

  tibble(
    .estimate = environment(model_stepfun)$x,
    .adj_estimate = environment(model_stepfun)$y
  )
}

# ------------------------- Isotonic Bootstrapped-------------------------------

#' @export
cal_isotonic_boot <- function(.data, truth = NULL, estimate = NULL, times = 10) {
  UseMethod("cal_isotonic_boot")
}

#' @export
cal_isotonic_boot.data.frame <- function(.data, truth = NULL,
                                         estimate = NULL, times = 10
                                         ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_boot(.data, !!truth, !!estimate, times = times)
    est <- as_cal_estimate(res, !! estimate)
  } else {
    stop_multiclass()
  }

  as_cal_object(est, !!truth, type, additional_class = "cal_isotonic_boot")
}

cal_isoreg_boot <- function(.data, truth, estimate, truth_val = NULL, times = 10) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  seeds <- sample.int(10000, times)
  mods <- purrr::map(
    seeds,
    ~ boot_iso(.data, !!truth, !!estimate, truth_val, .x)
    )
  boot_iso_cal(mods)
}

boot_iso <- function(.data, truth, estimate, truth_val, seed) {
  withr::with_seed(
    seed,
    {
      truth <- enquo(truth)
      estimate <- enquo(estimate)
      cal_isoreg_dataframe(.data, !!truth , !!estimate, truth_val, sampled = TRUE)
    }
  )
}

boot_iso_cal <- function(x) {
  # Creates 1,000 predictions using 0 to 1, which become the calibration
  new_estimates <- round(seq_len(1000) * 0.001, digits = 3)
  new_data <- data.frame(.estimate = new_estimates)
  new_probs <- map(x, ~ cal_add_interval(.x, .estimate, new_data))

  for(i in seq_along(new_probs)) {
    names(new_probs[[i]]) <- c(".estimate", paste0(".adj_", i))
  }

  merge_data <- purrr::reduce(new_probs, inner_join, by = ".estimate")

  adj_data <- dplyr::mutate(
    merge_data,
    .adj_estimate = rowMeans(dplyr::across(dplyr::contains(".adj_")))
    )

  dplyr::select(adj_data, .estimate, .adj_estimate)
}

# ---------------------------- Binary Objs--------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  cli::cli_div(theme = list(
    span.val0 = list(color = "blue"),
    span.val1 = list(color = "yellow")
  ))
  cli::cli_h2("Probability Calibration")
  cli::cli_text("Type: {.val0 Binary}")
  cli::cli_text("Method: {.val0 {x$method}}")
  cli::cli_text("Truth: {.val0 {x$truth}}")
  if(x$event_level == 1) {
    cli::cli_text("Levels: {.val1 {x$levels[[1]]}}{.val0 /{x$levels[[2]]}} ")
  } else {
    cli::cli_text("Levels: {.val0 {x$levels[[1]]}/}{.val1 {x$levels[[2]]}} ")
  }
  cli::cli_text("Estimate: {.val1 {names(x$estimates)}}")
  cli::cli_end()
}

# ----------------------------- Object Builders --------------------------------

as_cal_object <- function(estimates,
                          truth,
                          type,
                          .data,
                          event_level,
                          method,
                          additional_class = NULL) {
  lev <- process_level(event_level)
  truth <- enquo(truth)
  truth_name <- as_name(truth)
  levels <- levels(.data[, truth_name][[1]])
  structure(
    list(
      type = type,
      method = method,
      truth = truth_name,
      levels = levels,
      event_level = lev,
      estimates = estimates
    ),
    class = c("cal_object", paste0("cal_", type), additional_class)
  )
}

as_cal_estimate <- function(x, estimate) {
  estimate <- enquo(estimate)
  mod <- set_names(
    list(calibration = x),
    as_name(estimate)
  )
  mod
}

# ------------------------------- Utils ----------------------------------------

cal_logistic_impl <- function(.data,
                              truth = NULL,
                              estimate = NULL,
                              event_level = c("first", "second"),
                              type,
                              model,
                              method,
                              additional_class,
                              ...
                              ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_model_impl(
      .data = .data,
      truth = !!truth,
      estimate = !!estimate,
      event_level = event_level,
      method = model,
      ...
    )
    estimates <- as_cal_estimate(res, !! estimate)
  } else {
    stop_multiclass()
  }

  as_cal_object(
    estimates = estimates,
    truth = !!truth,
    type = type,
    method = method,
    .data = .data,
    event_level = event_level,
    additional_class = additional_class
  )
}

cal_model_impl <- function(.data, truth, estimate, method, event_level, ...) {
  truth <- ensym(truth)
  estimate <- ensym(estimate)

  if(method == "logistic_spline"){
    f_model <- expr(!!truth ~ s(!!estimate, k = 10))
    init_model <- mgcv::gam(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
    }
  if(method == "glm") {
    f_model <- expr(!!truth ~ !!estimate)
    init_model <- glm(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
    }

  model
}

add_is_val <- function(.data, truth, truth_val = NULL) {
  truth <- enquo(truth)
  if (!is.null(truth_val)) {
    ret <- mutate(
      .data,
      .is_val = ifelse(!!truth == !!truth_val, 1, 0)
    )
  } else {
    ret <- mutate(.data, .is_val = !!truth)
  }
  ret
}

is_binary_estimate <- function(estimate) {
  TRUE
}

stop_multiclass <- function() {
  stop("Multiclass not supported...yet")
}
