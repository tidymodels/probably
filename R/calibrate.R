#------------------------------- Logistic --------------------------------------
#' Uses a logistic model to calibrate probabilities
#' @param .data A data.frame object containing predictions and probability columns.
#' @param truth The column identifier for the true class results
#' (that is a factor). This should be an unquoted column name.
#' @param estimate A vector of column identifiers, or one of `dplyr` selector
#' functions to choose which variables contains the class probabilities. It
#' defaults to the prefix used by tidymodels (`.pred_`). The order of the
#' identifiers will be considered the same as the order of the levels of the
#' `truth` variable.
#' @param event_level  single string. Either "first" or "second" to specify which
#' level of truth to consider as the "event".
#' @param ... Optional arguments; currently unused.
#' @export
cal_logistic <- function(.data,
                         truth = NULL,
                         estimate = dplyr::starts_with(".pred_"),
                         event_level = c("first", "second"),
                         ...
                         ) {
  UseMethod("cal_logistic")
}

#' @export
cal_logistic.data.frame <- function(.data,
                                    truth = NULL,
                                    estimate = dplyr::starts_with(".pred_"),
                                    event_level = c("first", "second"),
                                    ...
                                    ) {

  cal_logistic_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    event_level = event_level,
    model = "glm",
    method = "Logistic",
    additional_class = "cal_logistic"
    )
}

#---------------------- Logistic Spline (GAM)  ---------------------------------
#' Uses a logistic spline model to calibrate probabilities
#' @inheritParams  cal_logistic
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
  cal_logistic_impl(
    .data = .data,
    truth = {{truth}},
    estimate = {{estimate}},
    event_level = event_level,
    model = "logistic_spline",
    method = "Logistic Spline",
    additional_class = "cal_logistic_spline"
  )
}

#------------------------------ Isotonic ---------------------------------------
#' Uses an Isotonic regression model to calibrate probabilities
#' @inheritParams cal_logistic
#' @export
cal_isotonic <- function(.data,
                         truth = NULL,
                         estimate = NULL,
                         event_level = c("first", "second"),
                         ...
                         ) {
  UseMethod("cal_isotonic")
}

#' @export
cal_isotonic.data.frame <- function(.data,
                                    truth = NULL,
                                    estimate = NULL,
                                    event_level = c("first", "second"),
                                    ...
                                    ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_dataframe(
      .data = .data,
      truth =  !!truth,
      estimate = !!estimate,
      truth_level = lev,
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
    method = "Isotonic",
    .data = .data,
    event_level = event_level,
    additional_class = "cal_isotonic"
  )
}

cal_isoreg_dataframe <- function(.data,
                                 truth,
                                 estimate,
                                 truth_level,
                                 sampled = FALSE,
                                 ...
                                 ) {

  truth <- enquo(truth)
  estimate <- enquo(estimate)

  sort_data <- dplyr::arrange(.data, !! estimate)

  if(sampled) {
    sort_data <- dplyr::slice_sample(
      .data = sort_data,
      prop = 1,
      replace = TRUE
      )
  }

  x <- dplyr::pull(sort_data, {{estimate}})

  truth <- dplyr::pull(sort_data, {{truth}})
  y <- as.integer(as.integer(truth) == truth_level)

  model <- isoreg(x = x, y = y)

  model_stepfun <- as.stepfun(model, ... = ...)

  dplyr::tibble(
    .estimate = environment(model_stepfun)$x,
    .adj_estimate = environment(model_stepfun)$y
  )
}

#-------------------------- Isotonic Bootstrapped-------------------------------

#' Uses a bootstrapped Isotonic regression model to calibrate probabilities
#' @param times Number of bootstraps.
#' @inheritParams cal_logistic
#' @export
cal_isotonic_boot <- function(.data,
                              truth = NULL,
                              estimate = NULL,
                              times = 10,
                              event_level = c("first", "second"),
                              ...
                              ) {
  UseMethod("cal_isotonic_boot")
}

#' @export
cal_isotonic_boot.data.frame <- function(.data,
                                         truth = NULL,
                                         estimate = NULL,
                                         times = 10,
                                         event_level = c("first", "second"),
                                         ...
                                         ) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  lev <- process_level(event_level)

  if(is_binary_estimate(!! estimate)) {
    type <- "binary"
    res <- cal_isoreg_boot(
      .data = .data,
      truth = !!truth,
      estimate = !!estimate,
      truth_level = lev,
      times = times
      )

    estimates <- as_cal_estimate(res, !! estimate)
  } else {
    stop_multiclass()
  }

  as_cal_object(
    estimates = estimates,
    truth = !!truth,
    type = type,
    method = "Isotonic Bootstrapped",
    .data = .data,
    event_level = event_level,
    additional_class = "cal_isotonic_boot"
  )
}

cal_isoreg_boot <- function(.data,
                            truth,
                            estimate,
                            truth_level,
                            times = 10
                            ) {

  sample.int(10000, times) %>%
    purrr::map(
      ~ boot_iso(
        .data = .data,
        truth =  {{truth}},
        estimate = {{estimate}},
        truth_level = truth_level,
        seed = .x
        )) %>%
    boot_iso_cal()
}

boot_iso <- function(.data, truth, estimate, truth_level, seed) {
  withr::with_seed(
    seed,
    {
      cal_isoreg_dataframe(
        .data = .data,
        truth = {{truth}} ,
        estimate = {{estimate}},
        truth_level = truth_level,
        sampled = TRUE
        )
    }
  )
}

boot_iso_cal <- function(x) {
  # Creates 1,000 predictions using 0 to 1, which become the calibration
  new_estimates <- seq(0, 1, by = 0.01)

  new_data <- data.frame(
    .estimate = new_estimates,
    .adj_estimate = new_estimates
    )

  new_probs <- purrr::map(
    x,
    ~ cal_add_interval(.x, .adj_estimate, new_data)
    )

  for(i in seq_along(new_probs)) {
    names(new_probs[[i]]) <- c(".estimate", paste0(".adj_", i))
  }

  merge_data <- new_probs %>%
    purrr::reduce(
      dplyr::inner_join, by = ".estimate"
    ) %>%
    dplyr::mutate(
      .adj_estimate = rowMeans(dplyr::across(dplyr::contains(".adj_")))
    ) %>%
    dplyr::select(.estimate, .adj_estimate)
}

#--------------------------------- Beta ----------------------------------------
cal_beta <- function(.data, truth = NULL, estimate = NULL, ...) {
  UseMethod("cal_beta")
}

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

#----------------------------- Binary Objs--------------------------------------

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

  if(is_binary_estimate(.data, !!truth)) {
    type <- "binary"
    te_map <- truth_estimate_map(.data, !! truth, {{ estimate }})
    estimate_1 <- te_map[[1]]
    res <- cal_model_impl(
      .data = .data,
      truth = !!truth,
      estimate = estimate_1,
      event_level = event_level,
      method = model,
      ...
    )
    estimates <- as_cal_estimate(res, !!estimate_1)
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

is_binary_estimate <- function(.data, truth) {
  l <- length(levels(dplyr::pull(.data, {{ truth }})))
  l == 2
}

stop_multiclass <- function() {
  cli::cli_abort("Multiclass not supported...yet")
}

truth_estimate_map <- function(.data, truth, estimate) {

  truth_str <- tidyselect::eval_select(
    expr = enquo(truth),
    data = .data[unique(names(.data))],
    allow_rename = FALSE
  )

  truth_levels <- levels(.data[[truth_str]])

  estimate_str <- tidyselect::eval_select(
    expr = enquo(estimate),
    data = .data[unique(names(.data))],
    allow_rename = FALSE
  ) %>%
    names()

  if (length(estimate_str) == 0) {
    cli::cli_abort("{.arg estimate} must select at least one column.")
  }

  if(all(substr(estimate_str, 1, 6) == ".pred_")) {
    est_map <- purrr::map(
      truth_levels,
      ~ sym(estimate_str[paste0(".pred_", .x) == estimate_str])
    )
  } else {
    est_map <- purrr::map(
      seq_along(truth_levels),
      ~ sym(estimate_str[[.x]])
    )
  }

  set_names(est_map, truth_levels)
}
