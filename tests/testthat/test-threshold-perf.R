sim_n <- 120

set.seed(1094)
ex_data <-
  data.frame(
    group_1 = sample(month.abb, replace = TRUE, size = sim_n),
    group_2 = sample(LETTERS[1:2], replace = TRUE, size = sim_n),
    outcome = factor(sample(paste0("Cl", 1:2), replace = TRUE, size = sim_n)),
    prob_est = runif(sim_n),
    x = rnorm(sim_n)
  )

ex_data_miss <- ex_data
ex_data_miss$group_1[c(1, 10, 29)] <- NA
ex_data_miss$prob_est[c(56, 117)] <- NA
ex_data_miss$outcome[c(49, 85, 57, 110)] <- NA

thr <- c(0, .5, .78, 1)

get_res <- function(prob, obs, cut) {
  cls <- recode_data(obs, prob, cut, event_level = "first")
  dat <- data.frame(
    obs = obs,
    cls = cls
  )

  mets <- yardstick::metric_set(sens, spec, j_index)

  .data_metrics <- dat %>%
    mets(obs, estimate = cls)

  # Create the `distance` metric data frame
  sens_vec <- .data_metrics %>%
    dplyr::filter(.metric == "sens") %>%
    dplyr::pull(.estimate)

  dist <- .data_metrics %>%
    dplyr::filter(.metric == "spec") %>%
    dplyr::mutate(
      .metric = "distance",
      # .estimate is spec currently
      .estimate = (1 - sens_vec) ^ 2 + (1 - .estimate) ^ 2
    )

  dplyr::bind_rows(.data_metrics, dist)
}

# ----------------------------------------------------------------

test_that('factor from numeric', {
  new_fac_1 <-
    recode_data(
      obs = ex_data$outcome,
      prob = ex_data$prob_est,
      threshold = ex_data$prob_est[1],
      event_level = "first"
    )
  tab_1 <- table(new_fac_1)
  expect_s3_class(new_fac_1, "factor")
  expect_true(isTRUE(all.equal(levels(new_fac_1), levels(ex_data$outcome))))
  expect_equal(unname(tab_1["Cl1"]), sum(ex_data$prob_est >= ex_data$prob_est[1]))
  expect_equal(unname(tab_1["Cl2"]), sum(ex_data$prob_est <  ex_data$prob_est[1]))

  # missing data
  new_fac_2 <-
    recode_data(
      obs = ex_data_miss$outcome,
      prob = ex_data_miss$prob_est,
      threshold = ex_data_miss$prob_est[1],
      event_level = "first"
    )
  tab_2 <- table(new_fac_2)
  expect_s3_class(new_fac_2, "factor")
  cmpl_probs <- ex_data_miss$prob_est[!is.na(ex_data_miss$prob_est)]
  expect_true(isTRUE(all.equal(is.na(new_fac_2), is.na(ex_data_miss$prob_est))))
  expect_true(isTRUE(all.equal(levels(new_fac_2), levels(ex_data_miss$outcome))))
  expect_equal(unname(tab_2["Cl1"]), sum(cmpl_probs >= ex_data_miss$prob_est[1]))
  expect_equal(unname(tab_2["Cl2"]), sum(cmpl_probs <  ex_data_miss$prob_est[1]))

  new_fac_3 <-
    recode_data(
      obs = ex_data$outcome,
      prob = ex_data$prob_est,
      threshold = ex_data$prob_est[1],
      event_level = "second"
    )
  tab_3 <- table(new_fac_3)
  expect_s3_class(new_fac_3, "factor")
  expect_true(isTRUE(all.equal(levels(new_fac_3), levels(ex_data$outcome))))
  expect_equal(unname(tab_3["Cl1"]), sum(ex_data$prob_est <  ex_data$prob_est[1]))
  expect_equal(unname(tab_3["Cl2"]), sum(ex_data$prob_est >= ex_data$prob_est[1]))
})

test_that('single group', {
  one_group_data <-
    ex_data %>%
    dplyr::group_by(group_2) %>%
    threshold_perf(
      outcome,
      prob_est,
      thresholds = thr
    )

  for(i in thr) {
    one_group_data_obs <- one_group_data %>%
      dplyr::filter(group_2 == "A" & .threshold == i) %>%
      dplyr::select(-group_2, -.threshold) %>%
      as.data.frame()

    one_group_data_exp <-
      get_res(
        ex_data$prob_est[ex_data$group_2 == "A"],
        ex_data$outcome[ex_data$group_2 == "A"],
        i
      ) %>%
      as.data.frame()
    expect_equal(one_group_data_obs, one_group_data_exp)
  }
})

