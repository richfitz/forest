source("helper-forest.R")

context("Plotting")

## This is really hard to test; how do you tell if the plot has been
## successful?  We could generate an SVG or something and compare, but
## that is really difficult to do.

test_that("Coordinate calculation", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  tp <- forest:::plotting_coordinates(tr)

  ## Longer-term, extracting data should not be done this way; we'll
  ## directly create grobs I think.
  xy <- do.call(rbind, treeapply(tp, function(x) unlist(x$data)))
  rownames(xy) <- unlist(treeapply(tp, function(x) x$label))
  xy <- as.data.frame(xy)
  xy$is_tip <- as.logical(xy$is_tip)

  ## These are the columns we expect:
  cols <- c("time_tipward", "time_rootward",
            "spacing_min", "spacing_max", "spacing_mid",
            "is_tip")
  expect_that(colnames(xy), equals(cols))

  ## Convert the 0..1 data to ape's scaling:
  s <- c("spacing_mid", "spacing_max", "spacing_min")
  xy[s] <- 1 + xy[s] * (tr$tips - 1)
  o <- rownames(xy)

  ## First, check time axis:
  cmp_time_tipward <- node.depth.edgelength(phy)
  names(cmp_time_tipward) <- c(phy$tip.label, phy$node.label)
  cmp_time_rootward <- cmp_time_tipward -
    phy$edge.length[match(seq_along(cmp_time_tipward), phy$edge[,2])]

  expect_that(xy$time_tipward,  equals(unname(cmp_time_tipward[o])))
  expect_that(xy$time_rootward, equals(unname(cmp_time_rootward[o])))

  ## Then, check spacing axis:
  cmp_spacing_mid <- node.height(phy)
  names(cmp_spacing_mid) <- names(cmp_time_tipward)
  expect_that(xy$spacing_mid,  equals(unname(cmp_spacing_mid[o])))

  xy_tips <- xy[xy$is_tip,]
  expect_that(xy_tips$spacing_mid, is_identical_to(xy_tips$spacing_min))
  expect_that(xy_tips$spacing_mid, is_identical_to(xy_tips$spacing_max))

  xy_nodes <- xy[!xy$is_tip,]

  cmp_spacing_range <-
    t(sapply(seq_len(phy$Nnode) + Ntip(phy),
             function(nd)
             range(cmp_spacing_mid[phy$edge[which(nd == phy$edge[,1]),2]])))
  rownames(cmp_spacing_range) <- phy$node.label
  o <- rownames(xy_nodes)

  expect_that(xy_nodes$spacing_min, equals(unname(cmp_spacing_range[o,1])))
  expect_that(xy_nodes$spacing_max, equals(unname(cmp_spacing_range[o,2])))

  ## This is all way nicer to look at than to compare directly.
  if (FALSE) {
    plot(phy)
    points(spacing_mid ~ time_tipward, xy, col="red")
    points(spacing_mid ~ time_rootward, xy, col="blue")
    points(spacing_min ~ time_tipward, xy, col="purple", cex=.5, pch=19)
    points(spacing_max ~ time_tipward, xy, col="green4", cex=.5, pch=19)
  }
})
