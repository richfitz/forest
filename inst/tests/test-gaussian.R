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

as.vector.gaussian <- function(g)
  c(mean=g$mean, variance=g$variance, log_scale=g$log_scale)

test_that("Gaussian defaults", {
  g <- new(forest:::gaussian)
  expect_that(g$mean,      is_identical_to(NA_real_))
  expect_that(g$variance,  is_identical_to(NA_real_))
  expect_that(g$log_scale, is_identical_to(NA_real_))
  expect_that(g$valid,     is_false())
})

test_that("Gaussian creation", {
  x <- c(mean=1, variance=2, log_scale=3)
  g <- new(forest:::gaussian, x[["mean"]], x[["variance"]],
           x[["log_scale"]])
  expect_that(g$mean,      is_identical_to(x[["mean"]]))
  expect_that(g$variance,  is_identical_to(x[["variance"]]))
  expect_that(g$log_scale, is_identical_to(x[["log_scale"]]))
  expect_that(g$valid,     is_true())

  g <- new(forest:::gaussian, x[["mean"]], -x[["variance"]],
           x[["log_scale"]])
  expect_that(g$valid,     is_false())
})

test_that("Gaussian product", {
  set.seed(1)
  x <- runif(3)
  y <- runif(3)
  names(x) <- names(y) <- c("mean", "variance", "log_scale")

  gx <- new(forest:::gaussian, x[1], x[2], x[3])
  gy <- new(forest:::gaussian, y[1], y[2], y[3])

  expect_that(as.vector.gaussian(gx$times(gy)),
              equals(gaussian.product(x, y)))

  ## Multiplication is commutative:
  expect_that(as.vector.gaussian(gx$times(gy)),
              is_identical_to(as.vector.gaussian(gy$times(gx))))
})

test_that("Brownian motion", {
  bm <- new(forest:::brownian_motion)
  expect_that(bm$parameters, is_identical_to(NA_real_))
  s2 <- exp(1)
  bm$parameters <- s2
  expect_that(bm$parameters, is_identical_to(s2))

  t <- pi

  ## Starting from delta function
  x <- new(forest:::gaussian, 0, 0, 0)
  cmp <- c(mean=x$mean, variance=x$variance + t * s2,
           log_scale=x$log_scale)

  expect_that(as.vector.gaussian(bm$forward(x, t)),
              is_identical_to(cmp))
  expect_that(as.vector.gaussian(bm$forward(x, t)),
              is_identical_to(cmp))

  ## Starting from gaussian
  v0 <- sqrt(2)
  set.seed(1)
  x <- new(forest:::gaussian, runif(1), v0, runif(1))
  cmp <- c(mean=x$mean, variance=x$variance + t * s2,
           log_scale=x$log_scale)

  expect_that(as.vector.gaussian(bm$forward(x, t)),
              is_identical_to(cmp))
  expect_that(as.vector.gaussian(bm$forward(x, t)),
              is_identical_to(cmp))
})

test_that("Brownian motion combine", {
  bm <- new(forest:::brownian_motion)
  bm$parameters <- exp(1)

  set.seed(1)
  x <- runif(3)
  y <- runif(3)
  names(x) <- names(y) <- c("mean", "variance", "log_scale")

  gx <- new(forest:::gaussian, x[1], x[2], x[3])
  gy <- new(forest:::gaussian, y[1], y[2], y[3])

  expect_that(as.vector.gaussian(bm$combine(gx, gy)),
              equals(gaussian.product(x, y)))
})

test_that("Push Gaussians onto tree", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  states <- structure(as.list(runif(tr$tips)), names=tr$tip_labels)
  tr$associate_data(states, TRUE, FALSE)

  gtr <- forest:::build_gaussian_tree(tr)

  cmp <- gtr$to_rtree()

  expect_that(treeapply(cmp, function(x) x$label),
              is_identical_to(treeapply(tr, function(x) x$label)))
  expect_that(treeapply(cmp, function(x) x$length),
              is_identical_to(treeapply(tr, function(x) x$length)))
  ## Here are the gaussians we made:
  gg <- treeapply(cmp, function(x) x$data)

  cmp <- treeapply(tr, function(x) x$data)
  is.node <- sapply(cmp, is.null)
  is.tip  <- !is.node

  expect_that(lapply(gg[is.tip], function(x) x$tipward$mean),
              is_identical_to(cmp[is.tip]))
  expect_that(lapply(gg[is.node], function(x) x$tipward$mean),
              is_identical_to(rep_len(list(NA_real_), sum(is.node))))
  expect_that(sapply(gg, function(x) x$tipward$variance),
              is_identical_to(ifelse(is.tip, 0.0, NA_real_)))
  expect_that(sapply(gg, function(x) x$tipward$log_scale),
              is_identical_to(ifelse(is.tip, 0.0, NA_real_)))

  expect_that(sapply(gg, function(x) x$rootward$mean),
              is_identical_to(rep_len(NA_real_, tr$size)))
  expect_that(sapply(gg, function(x) x$rootward$variance),
              is_identical_to(rep_len(NA_real_, tr$size)))
  expect_that(sapply(gg, function(x) x$rootward$log_scale),
              is_identical_to(rep_len(NA_real_, tr$size)))
})

test_that("BM calculations work", {
  set.seed(1)
  phy <- rtree(100)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)
  states <- structure(as.list(runif(tr$tips)), names=tr$tip_labels)
  tr$associate_data(states, TRUE, FALSE)
  gtr <- forest:::build_gaussian_tree(tr)

  bm <- new(forest:::brownian_motion)
  s2 <- exp(1)
  bm$parameters <- s2

  g.root <- forest:::all_branches_bm(gtr, bm)

  ## Copare against diversitree's calculations:
  lik <- make.bm(phy, unlist(states), control=list(method="pruning"))
  res <- attr(lik(s2, intermediates=TRUE), "intermediates")

  d.root <- res$vals

  expect_that(g.root$mean,      equals(d.root[[1]]))
  expect_that(g.root$variance,  equals(d.root[[2]]))
  expect_that(g.root$log_scale, equals(sum(res$lq) + d.root[[3]]))

  calculator <- new(forest:::calculator_bm, bm, gtr)
  c.root <- calculator$all_branches()

  expect_that(c.root$mean,      is_identical_to(g.root$mean))
  expect_that(c.root$variance,  is_identical_to(g.root$variance))
  expect_that(c.root$log_scale, is_identical_to(g.root$log_scale))

  ## Compare all the intermediates.
  ##
  ## This suggests we're doing a copy somewhere we should not; the
  ## tree did not end up modified.
  cmp <- gtr$to_rtree()
  gg <- treeapply(cmp, function(x) x$data)
  expect_that(all(is.na(sapply(gg, function(x) x$rootward$mean))),
              is_true())
})

gc()
