context("test-class-pred")

library(vctrs)

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
  cp4  <- class_pred(factor(c("a", "b", "b", "c"), ordered = TRUE), which = 2)

  # lossy cast, no c level in cp2
  expect_equal(
    expect_warning(
      vec_data(vec_cast(cp1, cp2))
    ),
    c(1, 0, 2, NA)
  )

  # casting to new class_pred preserves new eq label
  expect_equal(get_equivocal_label(vec_cast(cp1, cp3)), "eq")

  expect_true(is_ordered(vec_cast(cp1, cp4)))
})

test_that("casting class_pred to factor", {

  cp1  <- class_pred(factor(c("a", "b", "b", "c")))
  cp2  <- class_pred(factor(c("a", "b", "b", "c")), which = 2)
  cp3  <- class_pred(factor(c(NA, "a", "b", "c")), which = 3)
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

  # convert to factor with NA already present is not lossy
  expect_warning(vec_cast(cp3, factor()), NA)

  # special test for when the factor you are casting to has different
  # levels and they are in an odd order compared to what the class_pred had
  # (poor is between good and great here)
  cp_special  <- class_pred(factor(c("good", "great")))
  fc_special  <- factor(c("good", "poor", "great"), c("good", "poor", "great"))
  res_special <- factor(c("good", "great"), levels = c("good", "poor", "great"))
  expect_equal(vec_cast(cp_special, fc_special), res_special)

})

test_that("casting character to class_pred", {

  chr1 <- c("a", "b", "b", "c")
  cp1  <- class_pred(factor(c("a", "b", "b", "c")))
  cp2  <- class_pred(factor(c("a", "b", "b", "b")))
  cp3  <- class_pred(factor(c("a", "b", "b", "c"), ordered = TRUE))
  cp4  <- class_pred(factor(c("a", "b", "b", "c")), equivocal = "eq")

  # lossy cast, no c level in chr1
  expect_equal(
    expect_warning(
      vec_data(vec_cast(chr1, cp2))
    ),
    c(1, 2, 2, NA)
  )

  # equivocal label is maintained
  expect_equal(get_equivocal_label(vec_cast(chr1, cp4)), "eq")

  # clean conversion to class_pred
  expect_equal(vec_cast(chr1, cp1), class_pred(factor(c("a", "b", "b", "c"))))

  # converting to ordered class_pred maintains orderedness
  expect_true(is_ordered_class_pred(vec_cast(chr1, cp3)))

})

test_that("slicing", {

  # levels are kept
  expect_equal(levels(manual_creation_eq[1]), real_labs)

  # reportable rate updates
  expect_equal(reportable_rate(manual_creation_eq[1]), 0)
  expect_equal(reportable_rate(manual_creation_eq[1:2]), 0.5)

  # extending past adds NAs
  expect_equal(vec_data(manual_creation_eq[1:6]), c(0, 1, 2, 2, 3, NA))

})

test_that("unknown casts are handled correctly", {

  # numeric -> class_pred = error
  expect_error(vec_cast(numeric(), class_pred()))

  # logical vec -> class pred = depends on if only NA or has TRUE/FALSE
  expect_equal(vec_data(vec_cast(NA, class_pred())), NA_real_)
  expect_error(vec_data(vec_cast(TRUE, class_pred())))

  # NULL second = x, NULL first = NULL
  expect_equal(vec_cast(NULL, class_pred()), NULL)
  expect_equal(vec_cast(class_pred(), NULL), class_pred())

})

test_that("combining class preds", {

  cp1 <- new_class_pred(c(1L, 2L, 2L), c("low", "med"))
  cp2 <- new_class_pred(c(1L, 2L, 2L), c("low", "med"), ordered = TRUE)
  cp3 <- new_class_pred(c(1L, 2L, 2L), c("low", "med"), equivocal = "eq")

  # joining with different levels is the union
  expect_equal(levels(c(manual_creation_eq, cp1)), c("low", "med", "high"))

  # joining with different levels is the union in the order given
  # even if one is ordered
  expect_equal(levels(c(manual_creation_eq, cp2)), c("low", "med", "high"))
  expect_true(is_ordered_class_pred(c(manual_creation_eq, cp2)))

  # joining with different equivocal labels uses the LHS label
  expect_equal(get_equivocal_label(c(cp1, cp3)), "[EQ]")
  expect_equal(get_equivocal_label(c(cp3, cp1)), "eq")

})

test_that("combining class pred with factor", {

  cp1 <- class_pred(factor(c("good", "poor")), which = 2)
  chr <- c("good", "great", NA)
  fc1 <- factor(chr)

  join1 <- class_pred(
    factor(
      c("good", "poor", "good", "great", NA),
      levels = c("good", "poor", "great")
    ),
    which = 2
  )

  join2 <- class_pred(
    factor(
      c("good", "great", NA, "good", "poor"),
      levels = c("good", "great", "poor")
    ),
    which = 5
  )

  join3 <- c(1, 2, NA, 1, 0)

  # vec_c() joins are bidirectionally correct
  expect_equal(vec_c(cp1, fc1), join1)
  expect_equal(vec_c(fc1, cp1), join2)

  # c() joins are correct if vctrs_vctr is first
  expect_equal(c(cp1, fc1), join1)

  # sadly this happens and cannot be overriden
  expect_equal(c(fc1, cp1), join3)
})

test_that("common type: factor and class_pred", {

  expect_is(vec_type2(class_pred(), factor()), "class_pred")

  expect_equal(
    levels(
      vec_type2(
        class_pred(factor(levels = "a")),
        factor(levels = "b")
      )
    ),
    c("a", "b")
  )

  expect_equal(
    get_equivocal_label(
      vec_type2(
        class_pred(factor(levels = "a"), equivocal = "eq"),
        factor(levels = "b")
      )
    ),
    "eq"
  )

  # reverse order

  expect_is(vec_type2(factor(), class_pred()), "class_pred")

  expect_equal(
    levels(
      vec_type2(
        factor(levels = "b"),
        class_pred(factor(levels = "a"))
      )
    ),
    c("b", "a")
  )

  expect_equal(
    get_equivocal_label(
      vec_type2(
        factor(levels = "b"),
        class_pred(factor(levels = "a"), equivocal = "eq")
      )
    ),
    "eq"
  )

})
