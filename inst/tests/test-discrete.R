source("helper-forest.R")

context("Discrete")

as.R.discrete <- function(d)
  list(log_scale=d$log_scale, probabilities=d$probabilities)

discrete.product <- function(x, y)
  list(log_scale=x$log_scale + y$log_scale,
       probabilities=x$probabilities * y$probabilities)

test_that("Discrete defaults", {
  d <- new(forest:::discrete)
  expect_that(d$size,          equals(0))
  expect_that(d$probabilities, equals(numeric(0)))
  expect_that(d$log_scale,     is_identical_to(NA_real_))
  expect_that(d$valid,         is_false())
})

test_that("Discrete resize", {
  d <- new(forest:::discrete)
  d$resize(2)
  expect_that(d$size,          equals(2))
  expect_that(d$probabilities, equals(rep(NA_real_, 2)))
  expect_that(d$log_scale,     is_identical_to(NA_real_))
  expect_that(d$valid,         is_false())
})

test_that("Discrete creation (vector + scale)", {
  p <- c(.1, .2)
  s <- 3

  d <- new(forest:::discrete, c(p, s))
  expect_that(d$size,          equals(2))
  expect_that(d$probabilities, is_identical_to(p))
  expect_that(d$log_scale,     is_identical_to(s))
  expect_that(d$valid,         is_true())

  d <- new(forest:::discrete, c(-p, s))
  expect_that(d$size,          equals(2))
  expect_that(d$probabilities, is_identical_to(-p))
  expect_that(d$log_scale,     is_identical_to(s))
  expect_that(d$valid,         is_false())

  d <- new(forest:::discrete, c(rep(0, 2), s)  )
  expect_that(d$valid,         is_false())

  d <- new(forest:::discrete, c(c(1e-8, 0), s))
  expect_that(d$valid,         is_true())
})

test_that("Discrete creation (vector + scale)", {
  p <- c(.1, .2)
  s <- 3

  d <- new(forest:::discrete, p, s)
  expect_that(d$size,          equals(2))
  expect_that(d$probabilities, is_identical_to(p))
  expect_that(d$log_scale,     is_identical_to(s))
  expect_that(d$valid,         is_true())

  d <- new(forest:::discrete, -p, s)
  expect_that(d$size,          equals(2))
  expect_that(d$probabilities, is_identical_to(-p))
  expect_that(d$log_scale,     is_identical_to(s))
  expect_that(d$valid,         is_false())

  d <- new(forest:::discrete, rep(0, 2), s)
  expect_that(d$valid,         is_false())

  d <- new(forest:::discrete, c(1e-8, 0), s)
  expect_that(d$valid,         is_true())

  ## Coerner cases:
  expect_that(new(forest:::discrete, numeric(0)), throws_error())

  d <- new(forest:::discrete, s)
  expect_that(d$size,          equals(0))
  expect_that(d$probabilities, equals(numeric(0)))
  expect_that(d$log_scale,     is_identical_to(s))
  expect_that(d$valid,         is_false())
})

test_that("Probability product", {
  set.seed(1)
  p1 <- runif(3)
  p2 <- runif(3)
  s1 <- rnorm(1)
  s2 <- rnorm(1)

  d1 <- new(forest:::discrete, p1, s1)
  d2 <- new(forest:::discrete, p2, s2)

  dp <- d1$times(d2)
  expect_that(as.R.discrete(dp), equals(discrete.product(d1, d2)))

  ## Multiplication is commutative:
  expect_that(as.R.discrete(d1$times(d2)),
              is_identical_to(as.R.discrete(d2$times(d1))))
})
