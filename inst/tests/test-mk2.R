source("helper-forest.R")

source("support-mk.R")

context("Mk2")

test_that("Mk2", {
  m <- new(forest:::mk2)
  expect_that(m$parameters, is_identical_to(rep(NA_real_, 2)))
  pars <- runif(2)
  m$parameters <- pars
  expect_that(m$parameters, is_identical_to(pars))
})

## Basic check; just run some random data forwards and backwards with
## the Mk2 model and compare with the reference implementation.
test_that("Mk2 agrees with reference implementation", {
  tt <- seq(0, 5, length=31)
  pars <- c(.5, 1)
  set.seed(1)
  y <- runif(2)
  s <- runif(1)

  d <- new(forest:::discrete, y, s)
  m <- new(forest:::mk2)
  m$parameters <- pars

  expect_that(t(sapply(tt, function(t) m$forward(d, t)$probabilities)),
              equals(mk2.forward(y, tt, pars)))
  expect_that(t(sapply(tt, function(t) m$backward(d, t)$probabilities)),
              equals(mk2.backward(y, tt, pars)))
  expect_that(m$backward(d, 10)$log_scale, is_identical_to(s))
})

test_that("Mk2 corner cases", {
  s <- runif(1)
  d0 <- new(forest:::discrete, c(1, 0), s)
  d1 <- new(forest:::discrete, c(0, 1), s)
  d2 <- new(forest:::discrete, c(.5, .5), s)
  d3 <- new(forest:::discrete, c(0, 0), s)

  ## If q01 is zero, then we never move from 0->1 (but 1->0
  ## transitions are allowed).
  pars <- c(0, 0.1)
  m <- new(forest:::mk2)
  m$parameters <- pars

  ## With no time, nothing changes:
  expect_that(m$forward(d0, 0)$probabilities,
              is_identical_to(d0$probabilities))
  expect_that(m$forward(d1, 0)$probabilities,
              is_identical_to(d1$probabilities))
  expect_that(m$forward(d2, 0)$probabilities,
              is_identical_to(d2$probabilities))
  expect_that(m$forward(d3, 0)$probabilities,
              is_identical_to(d3$probabilities))

  ## Forward time simulation:
  t <- 10
  ## Can't move away from state 0, so d0 unchanged:
  expect_that(m$forward(d0, t)$probabilities,
              is_identical_to(d0$probabilities))
  ## But starting in state 1 we can move to state 0 fine
  p <- m$forward(d1, t)$probabilities
  expect_that(min(p) > 0 && max(p) < 1, is_true())
  ## And eventually that's basically the only place that we can be:
  p <- m$forward(d1, 100 * t)$probabilities
  expect_that(1 - p[1] < 1e-8 & p[2] < 1e-8, is_true())
  ## Doing this from an unknown state:
  p <- m$forward(d2, t)$probabilities
  expect_that(min(p) > 0 && max(p) < 1, is_true())
  p <- m$forward(d2, 100 * t)$probabilities
  expect_that(1 - p[1] < 1e-8 & p[2] < 1e-8, is_true())
  ## And in an impossible situation (no state valid)
  expect_that(m$forward(d3, t)$probabilities,
              is_identical_to(d3$probabilities))

  ## Backward time
  ## If we were in state 0 we cannot have changed so that explains the
  ## data fine.  Otherwise we can move there, which would also explain it.
  p <- m$backward(d0, t)$probabilities
  expect_that(p[1], is_identical_to(1))
  expect_that(p[2] > 0, is_true())
  ## But there is no way of explaining the data if we are in state 1
  ## unless we started there.
  p <- m$backward(d1, t)$probabilities
  expect_that(p[1], is_identical_to(0))
  expect_that(p[2] > 0, is_true())
  ## Doing this from an unknown state:
  expect_that(m$backward(d2, t)$probabilities,
              is_identical_to(d2$probabilities))
  ## And in an impossible situation (no state valid)
  expect_that(m$backward(d3, t)$probabilities,
              is_identical_to(d3$probabilities))
})
