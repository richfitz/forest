library(expm)

## I'm not sure if this is the best way to do this, but I need some
## support functions for the Mk models that are not used elsewhere.

## Build a 2x2 Q-matrix from the two transition rates.
make.Q <- function(pars) {
  stopifnot(length(pars) == 2)
  q01 <- pars[1]
  q10 <- pars[2]
  rbind(c(-q01, q01), c(q10, -q10))
}

## Mk2 specifically:
derivs.backward.2 <- function(t, y, pars) {
  q01 <- pars[1]
  q10 <- pars[2]
  list(c(-q01 * y[1] + q01 * y[2],
         -q10 * y[2] + q10 * y[1]))
}
derivs.forward.2 <- function(t, y, pars) {
  q01 <- pars[1]
  q10 <- pars[2]
  list(c(-q01 * y[1] + q10 * y[2],
         -q10 * y[2] + q01 * y[1]))
}

## Mk generally:
derivs.backward <- function(t, y, Q)
  list(drop(Q %*% y))
derivs.forward <- function(t, y, Q)
  list(drop(y %*% Q))

## Mk models solved using odes:
mk2.ode <- function(y, t, pars, derivs)
  unname(lsoda(y, t, derivs, pars, rtol=1e-9, atol=1e-9)[,-1])
mk2.backward.ode <- function(y, t, pars)
  mk2.ode(y, t, pars,
          if(is.matrix(pars)) derivs.backward else derivs.backward.2)
mk2.forward.ode <- function(y, t, pars)
  mk2.ode(y, t, pars,
          if(is.matrix(pars)) derivs.forward else derivs.forward.2)

## Mk models solved using exponentiation:
mk2.backward.expm <- function(y, t, Q)
  t(sapply(t, function(ti) drop(expm(Q * ti) %*% y)))
mk2.forward.expm <- function(y, t, Q)
  t(sapply(t, function(ti) drop(y %*% expm(Q * ti))))

## Mk2 models solved analytically.
##
## Mathematica code to do this:
## ansb = DSolve[{D[D0[t],t] == -q01 * D0[t] + q01 * D1[t],
##                D[D1[t],t] == -q10 * D1[t] + q10 * D0[t],
##                D0[0] == y0,
##                D1[0] == y1}, {D0[t], D1[t]}, t];
## ansf = DSolve[{D[D0[t],t] == -q01 * D0[t] + q10 * D1[t],
##               D[D1[t],t] == -q10 * D1[t] + q01 * D0[t],
##               D0[0] == y0,
##               D1[0] == y1}, {D0[t], D1[t]}, t];
mk2.backward <- function(y, t, pars) {
  q01 <- pars[[1]]
  q10 <- pars[[2]]
  y0 <- y[[1]]
  y1 <- y[[2]]

  qq <- q01 + q10
  eqqt <- exp(-qq * t)
  tmp1 <- q10 * y0 + q01 * y1

  cbind((tmp1 + q01*(y0 - y1) * eqqt) / qq,
        (tmp1 + q10*(y1 - y0) * eqqt) / qq)
}

mk2.forward <- function(y, t, pars) {
  q01 <- pars[[1]]
  q10 <- pars[[2]]
  y0 <- y[[1]]
  y1 <- y[[2]]

  qq <- q01 + q10
  eqqt <- exp(qq * t)

  cbind((q01 * y0 + q10 * (eqqt * (y0 + y1) - y1)) / (eqqt * qq),
        (q10 * y1 + q01 * (eqqt * (y0 + y1) - y0)) / (eqqt * qq))
}
