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

test_that("Brownian motion", {
  bm <- new(forest:::brownian_motion)
  expect_that(bm$parameters, is_identical_to(0.0))
  s2 <- exp(1)
  bm$parameters <- s2
  expect_that(bm$parameters, is_identical_to(s2))

  x <- c(mean=0, variance=0, scale=1)
  t <- pi
  expect_that(bm$forward(x, t),
              is_identical_to(c(x[1], variance=t * s2, x[3])))
  expect_that(bm$backward(x, t),
              is_identical_to(c(x[1], variance=t * s2, x[3])))

  v0 <- sqrt(2)
  set.seed(1)
  x <- c(mean=runif(1), variance=v0, scale=runif(1))
  expect_that(bm$forward(x, t),
              is_identical_to(c(x[1], variance=v0 + t * s2, x[3])))
  expect_that(bm$backward(x, t),
              is_identical_to(c(x[1], variance=v0 + t * s2, x[3])))
})
