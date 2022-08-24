test_data <- segment_logistic[1:5,]
good <- test_data$.pred_good
poor <- test_data$.pred_poor
lvls <- levels(test_data$Class)

test_data2 <- species_probs[1:5,]
bobcat <- test_data2$.pred_bobcat
coyote <- test_data2$.pred_coyote
gray_fox <- test_data2$.pred_gray_fox
lvls2 <- levels(test_data2$Species)

test_that("two class succeeds with vector interface", {

  res <- make_two_class_pred(good, levels = lvls, threshold = .5, buffer = .4)

  fct <- factor(c("poor", "poor", "good", "good", "poor"))
  known <- class_pred(fct, which = c(2,3,4))

  expect_s3_class(res, "class_pred")
  expect_equal(res, known)

})

test_that("multi class succeeds with vector interface", {

  res <- make_class_pred(bobcat, coyote, gray_fox, levels = lvls2, min_prob = 0.5)

  fct <- factor(c("gray_fox", "gray_fox", "bobcat", "gray_fox", "coyote"))
  known <- class_pred(fct, which = 5)

  expect_s3_class(res, "class_pred")
  expect_equal(res, known)

})

test_that("multi class succeeds with data frame helper", {

  res <- append_class_pred(
    test_data2,
    contains(".pred_"),
    levels = lvls2,
    min_prob = 0.5,
    name = "cp_test"
  )

  known <- make_class_pred(bobcat, coyote, gray_fox, levels = lvls2, min_prob = 0.5)

  expect_s3_class(res, "data.frame")
  expect_equal(res[["cp_test"]], known)

})



test_that("ordered passes through to class_pred", {

  res <- make_class_pred(bobcat, coyote, gray_fox, levels = lvls2, ordered = TRUE)
  res2 <- make_class_pred(bobcat, coyote, gray_fox, levels = lvls2, ordered = TRUE)

  expect_true(is_ordered_class_pred(res))
  expect_true(is_ordered_class_pred(res2))

})

test_that("fails with different length `...`", {

  v1 <- c(1, 2, 3)
  v2 <- c(1, 2)

  expect_error(
    make_class_pred(v1, v2),
    "All vectors passed to `...` must be of the same length."
  )

})

test_that("fails with different type `...`", {

  v1 <- c(1)
  v2 <- c("a")

  expect_error(
    make_class_pred(v1, v2),
    "At least one vector supplied to `...` is not numeric: 2"
  )

})

test_that("fails with different length `...` VS levels", {

  v1 <- c(1, 2, 3)
  v2 <- c(1, 2, 3)

  expect_error(
    make_class_pred(v1, v2, levels = c("one", "two", "three")),
    "`levels` must be a character vector with the same length as the number of vectors passed to `...`."
  )

})

test_that("validates type of `levels` (#42)", {
  expect_error(
    make_two_class_pred(1, levels = NULL),
    "`levels` must be a character vector of length 2."
  )
  expect_error(
    make_class_pred(1, 2, levels = c(0L, 4L)),
    "`levels` must be a character vector"
  )
})
