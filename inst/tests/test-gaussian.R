source("helper-forest.R")

context("Gaussian")

test_that("Gaussian transport wrappers", {
  x <- c(mean=1, variance=2, scale=3)

  expect_that(forest:::test_gaussian(x[1]),
              throws_error())
  expect_that(forest:::test_gaussian(x[c(1,1:3)]),
              throws_error())
  expect_that(forest:::test_gaussian(x),
              is_identical_to(unname(x)))
})
