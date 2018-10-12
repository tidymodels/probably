context("test-class-pred")

# ------------------------------------------------------------------------------
# Setup

ex_eq    <- c(0L, 1L, 2L, 2L, 3L)
ex_no_eq <- c(1L, 1L, 2L, 2L, 3L)
real_labs <- c("low", "med", "high")

factor_no_eq <- factor(ex_no_eq, labels = real_labs)

manual_creation_eq <- new_class_pred(ex_eq, labels = real_labs)
manual_creation_no_eq <- new_class_pred(ex_no_eq, labels = real_labs)

masked_high <- class_pred(factor_no_eq, which = 5)

# ------------------------------------------------------------------------------
# Testing

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
  expect_error(class_pred(factor("[EQ]")))

  # changing the label means we can use "EQ"
  expect_error(class_pred(factor("[EQ]"), equivocal = "hi"), NA)
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

test_that("class_pred masking a level retains the level", {
  expect_equal(
    levels(masked_high),
    c("low", "med", "high")
  )
})

test_that("coercing class_pred to factor retains all levels", {
  expect_equal(
    levels(masked_high),
    levels(as.factor(masked_high))
  )
})

test_that("class_pred can be coerced to ordered factor", {
  expect_is(as.ordered(manual_creation_eq), "ordered")
})

test_that("casting class_pred to class_pred", {

  cp1  <- class_pred(factor(c("a", "b", "b", "c")), which = 2)
  cp2  <- class_pred(factor(c("a", "b", "b", "b")), which = 3)
  cp3  <- class_pred(factor(c("a", "b", "b", "c")), which = 2, equivocal = "eq")

  # lossy cast, no c level in cp2
  expect_equal(
    expect_warning(
      vec_data(vec_cast(cp1, cp2))
    ),
    c(1, 0, 2, NA)
  )

  # casting to new class_pred preserves new eq label
  expect_equal(get_equivocal_label(vec_cast(cp1, cp3)), "eq")
})

test_that("casting class_pred to factor", {

  cp1  <- class_pred(factor(c("a", "b", "b", "c")))
  cp2  <- class_pred(factor(c("a", "b", "b", "c")), which = 2)
  fc1  <- factor(levels = c("a", "b"))
  fc2  <- factor(levels = c("a", "b", "c"))
  fc3  <- factor(levels = c("a", "b", "c"), ordered = TRUE)

  # lossy cast, no c level in fc1
  expect_equal(
    expect_warning(
      vec_data(vec_cast(cp1, fc1))
    ),
    c(1, 2, 2, NA)
  )

  # clean conversion to factor
  expect_equal(vec_cast(cp1, fc2), factor(c("a", "b", "b", "c")))

  # convert to factor, eq becomes NA
  expect_equal(vec_cast(cp2, fc2), factor(c("a", NA, "b", "c")))

  # converting to ordered factor maintains orderedness
  expect_equal(vec_cast(cp1, fc3), ordered(c("a", "b", "b", "c")))

})
