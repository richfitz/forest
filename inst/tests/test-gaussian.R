source("helper-forest.R")

context("Gaussian")

gaussian.product <- function(x, y) {
  m.x <- x[[1]]
  v.x <- x[[2]]
  s.x <- x[[3]]

  m.y <- y[[1]]
  v.y <- y[[2]]
  s.y <- y[[3]]

  vv <- v.x + v.y

  c(mean=(m.x * v.y + m.y * v.x) / vv,
    variance=(v.x * v.y) / vv,
    log_scale=s.x + s.y -(m.x - m.y)^2 / (2 * vv) - log(2 * pi * vv) / 2)
}

test_that("Gaussian transport wrappers", {
  x <- c(mean=1, variance=2, log_scale=3)

  expect_that(forest:::test_gaussian(x[1]),
              throws_error())
  expect_that(forest:::test_gaussian(x[c(1,1:3)]),
              throws_error())
  expect_that(forest:::test_gaussian(x),
              is_identical_to(unname(x)))
})

test_that("Gaussian product", {
  set.seed(1)
  x <- runif(3)
  y <- runif(3)
  names(x) <- names(y) <- c("mean", "variance", "log_scale")
  expect_that(forest:::test_gaussian_product(x, y),
              equals(gaussian.product(x, y)))
})

test_that("Brownian motion", {
  bm <- new(forest:::brownian_motion)
  expect_that(bm$parameters, is_identical_to(0.0))
  s2 <- exp(1)
  bm$parameters <- s2
  expect_that(bm$parameters, is_identical_to(s2))

  x <- c(mean=0, variance=0, log_scale=0)
  t <- pi
  expect_that(bm$forward(x, t),
              is_identical_to(c(x[1], variance=t * s2, x[3])))
  expect_that(bm$backward(x, t),
              is_identical_to(c(x[1], variance=t * s2, x[3])))

  v0 <- sqrt(2)
  set.seed(1)
  x <- c(mean=runif(1), variance=v0, log_scale=runif(1))
  expect_that(bm$forward(x, t),
              is_identical_to(c(x[1], variance=v0 + t * s2, x[3])))
  expect_that(bm$backward(x, t),
              is_identical_to(c(x[1], variance=v0 + t * s2, x[3])))
})

test_that("Brownian motion combine", {
  bm <- new(forest:::brownian_motion)
  bm$parameters <- exp(1)

  set.seed(1)
  x <- runif(3)
  y <- runif(3)
  names(x) <- names(y) <- c("mean", "variance", "log_scale")

  expect_that(bm$combine(x, y),
              equals(gaussian.product(x, y)))
})
