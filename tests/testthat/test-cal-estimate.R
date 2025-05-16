test_that("Non-default names used for estimate columns", {
  skip_if_not_installed("modeldata")

  new_segment <- segment_logistic
  colnames(new_segment) <- c("poor", "good", "Class")

  set.seed(100)
  expect_snapshot(
    cal_estimate_isotonic(new_segment, Class, c(good, poor))
  )
})

test_that("Test exceptions", {
  expect_error(
    cal_estimate_isotonic(segment_logistic, Class, dplyr::starts_with("bad"))
  )
})

test_that("non-standard column names", {
  library(dplyr)
  # issue 145
  seg <- segment_logistic |>
    rename_with(~ paste0(.x, "-1"), matches(".pred")) |>
    mutate(
      Class = paste0(Class,"-1"),
      Class = factor(Class),
      .pred_class = ifelse(`.pred_poor-1` >= 0.5, "poor-1", "good-1")
    )
  calib <- cal_estimate_isotonic(seg, Class)
  new_pred <- cal_apply(seg, calib, pred_class = .pred_class)
  expect_named(new_pred, c(".pred_poor-1", ".pred_good-1", "Class", ".pred_class"))

})
