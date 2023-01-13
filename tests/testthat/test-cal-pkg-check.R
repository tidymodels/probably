test_that("Missing package returns error", {
  expect_error(
    cal_pkg_check(c("NotReal"))
  )
})
