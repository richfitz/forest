## This test file is a bit odd; it tests different ways of computing
## things for the Mk/Mk2 model -- but none of them are implemented in
## forest.  These are just for making sure that the underlying
## calculations are correct when computed different ways.

source("helper-forest.R")

source("support-mk.R")

context("Mk")

test_that("Mk models agree with different approaches", {
  set.seed(1)
  pars <- runif(2)
  Q <- make.Q(pars)
  y <- runif(2)
  tt <- seq(0, 10, length=101)

  expect_that(mk2.backward.ode(y, tt, pars),
              equals(mk2.backward.ode(y, tt, Q)))
  expect_that(mk2.backward.expm(y, tt, Q),
              equals(mk2.backward.ode(y, tt, Q)))
  expect_that(mk2.backward(y, tt, pars),
              equals(mk2.backward.ode(y, tt, Q)))

  expect_that(mk2.forward.ode(y, tt, pars),
              equals(mk2.forward.ode(y, tt, Q)))
  expect_that(mk2.forward.expm(y, tt, Q),
              equals(mk2.forward.ode(y, tt, Q)))
  expect_that(mk2.forward(y, tt, pars),
              equals(mk2.forward.ode(y, tt, Q)))
})
