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

  xy <- forest:::plotting_prepare(tr)
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
    points(spacing_mid ~ time_tipward,  xy, col="red")
    points(spacing_mid ~ time_rootward, xy, col="blue")
    points(spacing_min ~ time_tipward,  xy, col="purple", cex=.5, pch=19)
    points(spacing_max ~ time_tipward,  xy, col="green4", cex=.5, pch=19)
  }
})

test_that("treeGrob construction", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  for (direction in c("right", "left", "up", "down", "circle")) {
    vp <- viewport(name="extra", width=.5)
    tg <- treeGrob(tr, name="mytree", direction=direction, vp=vp)

    ## The tree grob is named:
    expect_that(tg$name, equals("mytree"))
    ## Viewport is associated:
    expect_that(tg$vp,   is_identical_to(vp))
    ## Direction is correct:
    expect_that(tg$direction, equals(direction))

    ## There is a viewport for scaling:
    expect_that(tg$childrenvp$name, equals("scaling"))

    ## These possibly could get relaxed:
    expect_that(names(tg$children), equals(c("seg_t", "seg_s")))

    expect_that(tg$children$seg_t, is_a("tree_seg_time"))
    expect_that(tg$children$seg_s, is_a("tree_seg_spacing"))

    expect_that(tg$children$seg_t$vp, is_identical_to(vpPath("scaling")))
    expect_that(tg$children$seg_s$vp, is_identical_to(vpPath("scaling")))

    ## Probably worth testing here that everything is good within
    ## these, but that could get hard.  These are relatively
    ## unmodified though.
    expect_that(names(tg$children$seg_t),
                equals(c("t0", "t1", "s", "label", "is_tip",
                         "direction", "name", "gp", "vp")))
    expect_that(names(tg$children$seg_s),
                equals(c("s0", "s1", "t", "label", "is_tip",
                         "direction", "name", "gp", "vp")))
    if (interactive()) {
      grid.newpage()
      popViewport(0)
      pushViewport(viewport(width=.8, height=.8, name="spacing"))
      grid.rect(gp=gpar(col="grey", lty=2))
      grid.draw(tg)
    }
  }
})

## Node and tip labels:
test_that("Labels", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)
  gp.tip <- gpar(col="red")
  gp.node <- gpar(col="blue")
  for (direction in c("right", "left", "up", "down", "circle")) {
    vp <- viewport(name="extra", width=.5)
    tg <- treeGrob(tr, name="mytree", direction=direction, vp=vp)
    tg <- add_tip_labels(tg,  gp=gp.tip)
    tg <- add_node_labels(tg, gp=gp.node)

    expect_that(names(tg$children),
                equals(c("seg_t", "seg_s",
                         "tip_labels", "node_labels")))
    expect_that(tg$children$tip_labels,  is_a("tree_label"))
    expect_that(tg$children$node_labels, is_a("tree_label"))
    expect_that(tg$children$tip_labels$gp, is_identical_to(gp.tip))
    expect_that(tg$children$node_labels$gp, is_identical_to(gp.node))

    if (interactive()) {
      grid.newpage()
      popViewport(0)
      pushViewport(viewport(width=.8, height=.8, name="spacing"))
      grid.rect(gp=gpar(col="grey", lty=2))
      grid.draw(tg)
    }
  }
})
