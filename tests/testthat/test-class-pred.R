context("test-class-pred")

ex_eq    <- c(0L, 1L, 2L, 2L, 3L)
ex_no_eq <- c(1L, 1L, 2L, 2L, 3L)
real_labs <- c("low", "med", "high")

factor_no_eq <- factor(ex_no_eq, labels = real_labs)

manual_creation_eq <- structure(ex_eq, labels = real_labs, class = c("class_pred", "vctrs_vctr"))
manual_creation_no_eq <- structure(ex_no_eq, labels = real_labs, class = c("class_pred", "vctrs_vctr"))


test_that("can create from integers", {
  expect_equal(new_class_pred(ex_eq, real_labs), manual_creation_eq)
  expect_equal(new_class_pred(ex_no_eq, real_labs), manual_creation_no_eq)

  expect_error(new_class_pred("not-an-integer", "lab"))
  expect_error(new_class_pred(1L, 0))
})

test_that("can create from helper", {
  expect_equal(class_pred(factor_no_eq, which = 1), manual_creation_eq)
  expect_equal(class_pred(factor_no_eq), manual_creation_no_eq)

  expect_error(class_pred(1L))
})

test_that("equivocal label is not allowed as level", {
  expect_error(class_pred(factor("EQ")))

  # changing the label means we can use "EQ"
  opt <- getOption("probably.equivocal_label")
  options(probably.equivocal_label = "?")
  on.exit(options(probably.equivocal_label = opt))

  expect_s3_class(class_pred(factor("EQ")), "class_pred")
})

test_that("format preserves equivocal values", {
  expect_equal(
    format(manual_creation_eq),
    c("[EQ]", "low", "med", "med", "high")
  )
})

test_that("levels() does not return the equivocal label", {
  expect_equal(levels(manual_creation_eq), c("low", "med", "high"))
})

