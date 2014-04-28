source("helper-forest.R")

context("Plotting (clade tree)")

test_that("Coordinate calculation", {
  plotting_prepare_clade <- forest:::plotting_prepare_clade
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  set.seed(1)
  n_taxa <- pmax(1, rpois(tr$tips, 3))
  names(n_taxa) <- tr$tip_labels

  xy0 <- forest:::plotting_prepare(tr)
  xy <- plotting_prepare_clade(tr, n_taxa, 0.5)

  # First, in contrast to plotting_prepare, we won't have spacing_min
  # equal spacing_max for tips:
  expect_that(all(xy0$spacing_max > xy0$spacing_min), is_false())
  expect_that(all(xy$spacing_max > xy$spacing_min), is_true())

  # This helper function reimplements the same algorithm for tip
  # placement as the compiled code (the compiled code was adapted from
  # this R code)
  clade_info <- function(n_taxa, p) {
    r <- p * sum(n_taxa) + (1 - p) * (length(n_taxa) - 1)
    s_max_clade <- cumsum(n_taxa)
    s_min_clade <- c(0, s_max_clade[-length(n_taxa)])
    s_even <- seq_along(n_taxa) - 1L
    s_min <- (p * s_min_clade + (1 - p) * s_even) / r
    s_max <- (p * s_max_clade + (1 - p) * s_even) / r
    pos <- cbind(spacing_min=s_min,
                 spacing_mid=(s_min + s_max) / 2,
                 spacing_max=s_max)
    rownames(pos) <- names(n_taxa)
    pos
  }
  gc() # stupid Rcpp finalizer bug.

  pos.R <- clade_info(n_taxa, 0.5)
  pos.C <- as.matrix(xy[rownames(pos.R), colnames(pos.R)])
  expect_that(pos.C, equals(pos.R))

  # Now, repeat for a range of p values:
  for (p in seq(0, 1, length=11)) {
    pos.R <- clade_info(n_taxa, p)
    pos.C <- plotting_prepare_clade(tr, n_taxa, p)
    pos.C <- as.matrix(pos.C[rownames(pos.R), colnames(pos.R)])
    expect_that(pos.C, equals(pos.R))
  }

  # A couple of final checks.

  # When we set p -> 0, we should converge on the the non-clade
  # version:
  tmp <- plotting_prepare_clade(tr, n_taxa, 0)
  expect_that(tmp[names(tmp) != "is_clade"], equals(xy0))

  # When we set p -> 1 there should be no gaps between clades:
  pos.1 <- plotting_prepare_clade(tr, n_taxa, 1.0)
  pos.1 <- pos.1[pos.1$is_tip,]
  pos.1 <- pos.1[order(pos.1$spacing_mid),] # not strictly needed
  expect_that(pos.1$spacing_min[-1],
              equals(pos.1$spacing_max[-nrow(pos.1)]))

  # Graphically, this is what is going on:
  if (interactive()) {
    plot(NA, xlim=c(0, 1), ylim=c(0, 1))
    abline(h=c(0, 1), col="grey", lty=3)
    x <- rep(1, length(n_taxa))
    for (p in seq(0, 1, length=11)) {
      tmp <- clade_info(n_taxa, p)
      points(x * p, tmp[,"spacing_mid"], pch=19, col="red", cex=.4)
      suppressWarnings(arrows(x * p, tmp[,"spacing_min"],
                              x * p, tmp[,"spacing_max"],
                              code=3, length=0.03, angle=90))
    }
  }

  ## TODO: Probably worth checking with a vector that is all 1 and all
  ## >1 to make sure that the approach here is sane.
  expect_that(xy[names(n_taxa), "is_clade"],
              equals(unname(n_taxa > 1)))

  # Corner cases:

  # Sanity checking on the n_taxa vector.
  expect_that(plotting_prepare_clade(tr, unname(n_taxa), 0),
              throws_error("must be named"))
  expect_that(plotting_prepare_clade(tr, n_taxa[-1], 0),
              throws_error("Missing names"))
  expect_that(plotting_prepare_clade(tr, c(n_taxa[-1], foo=1), 0),
              throws_error("Missing names"))
  expect_that(plotting_prepare_clade(tr, c(n_taxa, foo=1), 0),
              throws_error("Unknown names"))

  expect_that(plotting_prepare_clade(tr, n_taxa - 1, 0),
              throws_error("at least 1"))
  expect_that(plotting_prepare_clade(tr, n_taxa - 1e-8, 0),
              throws_error("at least 1"))

  # The order of n_taxa does not matter:
  expect_that(plotting_prepare_clade(tr, sample(n_taxa), 0.5),
              is_identical_to(xy))
  expect_that(plotting_prepare_clade(tr, rev(n_taxa), 0.5),
              is_identical_to(xy))

  # Sanity checking on p:
  expect_that(plotting_prepare_clade(tr, n_taxa, numeric(0)),
              throws_error("scalar"))
  expect_that(plotting_prepare_clade(tr, n_taxa, c(.1, .2)),
              throws_error("scalar"))
  expect_that(plotting_prepare_clade(tr, n_taxa, -1),
              throws_error("between 0 and 1"))
  expect_that(plotting_prepare_clade(tr, n_taxa, 1.1),
              throws_error("between 0 and 1"))
  expect_that(plotting_prepare_clade(tr, n_taxa, "foo"),
              throws_error("between 0 and 1"))
})
