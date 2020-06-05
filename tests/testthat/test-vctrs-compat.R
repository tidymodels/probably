# ------------------------------------------------------------------------------
# vec_proxy_compare()

test_that("can take the comparison proxy", {
  x <- class_pred(factor(c("a", "b", NA)), which = 2)
  expect_identical(vec_proxy_compare(x), unclass(x))
})

# ------------------------------------------------------------------------------
# vctrs miscellaneous

test_that("can order by level with equivocal as smallest value using vec_order()", {
  x <- factor(c("a", "b", NA, "b"), levels = c("b", "a"))
  x <- class_pred(x, which = 2)

  expect <- factor(c("b", "b", "a", NA), levels = c("b", "a"))
  expect <- class_pred(expect, which = 1)

  expect_identical(x[vec_order(x)], expect)
})
