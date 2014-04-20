source("helper-forest.R")

# Actually getting a good taxonomic alignment is hard.  Lots of things
# don't play very well that way.

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
  if (interactive()) {
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

  for (direction in forest:::tree_directions()) {
    vp <- viewport(name="extra", width=.5)
    tg <- treeGrob(tr, name="mytree", direction=direction, vp=vp)

    ## The tree grob is named:
    expect_that(tg$name, equals("mytree"))
    ## Viewport is associated:
    expect_that(tg$vp,   is_identical_to(vp))
    ## Direction is correct:
    expect_that(tg$direction, equals(direction))

    ## Spacing info is included:
    expect_that(tg$spacing_info, is_a("list"))
    expect_that(tg$spacing_info$gaps, equals(tr$tips - 1))
    if (direction == "circle") {
      spacing_gap_size <- 2 * pi / tr$tips
      spacing_size     <- 2 * pi - spacing_gap_size
    } else if (direction == "semicircle") {
      spacing_gap_size <- pi / tg$spacing_info$gaps
      spacing_size     <- pi
    } else {
      spacing_gap_size <- 1 / tg$spacing_info$gaps
      spacing_size     <- 1
    }
    expect_that(tg$spacing_info$size,     equals(spacing_size))
    expect_that(tg$spacing_info$gap_size, equals(spacing_gap_size))

    ## There is a viewport for scaling:
    expect_that(tg$childrenvp$name, equals("scaling"))

    expect_that(names(tg$children), equals("branches"))
    expect_that(tg$children$branches, is_a("tree_branches"))
    expect_that(tg$children$branches$vp, is_identical_to(vpPath("scaling")))

    ## Probably worth testing here that everything is good within
    ## these, but that could get hard.  These are relatively
    ## unmodified though.
    expect_that(names(tg$children$branches),
                equals(c("label", "time_tipward", "time_rootward",
                         "spacing_min", "spacing_max", "spacing_mid",
                         "is_tip", "direction", "name", "gp", "vp")))
    if (interactive()) {
      vp.spacing <- viewport(width=.8, height=.8, name="spacing")
      print(tg, vp=vp.spacing)
      seekViewport("spacing")
      grid.rect(gp=gpar(col="grey", lty=2))
      seekViewport("extra")
      grid.rect(gp=gpar(col="steelblue3", lty=3))
      seekViewport("scaling")
      grid.rect(gp=gpar(col="red", lty=4))
    }
  }
})

test_that("tree_label_coords", {
  tree_label_coords <- forest:::tree_label_coords

  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)
  tg <- treeGrob(tr, direction="right")

  # Errors on invalid input
  expect_that(tree_label_coords("not_in_tree", tg), throws_error())
  # Empty input, empty output:
  expect_that(tree_label_coords(character(0), tg),
              equals(list(s=numeric(0), t=numeric(0))))

  label <- "t1"
  res <- tree_label_coords(label, tg)
  i <- tg$children$branches$label == label
  cmp <- list(s=tg$children$branches$spacing_mid[i],
              t=tg$children$branches$time_tip[i])
  expect_that(tree_label_coords(label, tg), equals(cmp))

  # Multiple labels:
  tip_labels <- tr$tip_labels
  res <- tree_label_coords(tip_labels, tg)
  i <- match(tip_labels, tg$children$branches$label)
  cmp <- list(s=tg$children$branches$spacing_mid[i],
              t=tg$children$branches$time_tip[i])
  expect_that(tree_label_coords(tip_labels, tg), equals(cmp))

  node_labels <- tr$node_labels
  res <- tree_label_coords(node_labels, tg)
  i <- match(node_labels, tg$children$branches$label)
  cmp <- list(s=tg$children$branches$spacing_mid[i],
              t=tg$children$branches$time_tip[i])
  expect_that(tree_label_coords(node_labels, tg), equals(cmp))

  # One label not in the tree within some that are:
  labels <- c(label, "not_in_tree")
  expect_that(tree_label_coords(labels, tg), throws_error())

  # Missing label values:
  expect_that(tree_label_coords(NA, tg), throws_error())
})

## NOTE: This is a basic test, but checks for the the simplest
## possible cases of the offset function.  When it comes time to
## offset in the spacing dimension too, this might be a a good place
## to start.
test_that("tree_label_coords_offset", {
  tree_offset <- forest:::tree_offset

  at <- list(s=1, t=1)
  offset <- unit(1, "lines")
  offset_reverse <- unit(-1, "lines")

  cmp_normal <- list(s=at$s,  t=unit(at$t, "native") + offset)
  cmp_reverse <- list(s=at$s, t=unit(at$t, "native") + offset_reverse)

  expect_that(tree_offset(at, offset, "right"),      equals(cmp_normal))
  expect_that(tree_offset(at, offset, "up"),         equals(cmp_normal))
  expect_that(tree_offset(at, offset, "circle"),     equals(cmp_normal))
  expect_that(tree_offset(at, offset, "semicircle"), equals(cmp_normal))

  expect_that(tree_offset(at, offset, "left"), equals(cmp_reverse))
  expect_that(tree_offset(at, offset, "down"), equals(cmp_reverse))
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

  # TODO: Test tip_labels() return values

  for (direction in forest:::tree_directions()) {
    vp <- viewport(name="extra", width=.5)
    tg <- treeGrob(tr, name="mytree", direction=direction, vp=vp) +
      tree_tip_labels(gp=gp.tip) +
      tree_node_labels(gp=gp.node)

    expect_that(names(tg$children),
                equals(c("branches", "tip_labels", "node_labels")))
    expect_that(tg$children$tip_labels,  is_a("tree_labels"))
    expect_that(tg$children$node_labels, is_a("tree_labels"))
    expect_that(tg$children$tip_labels$gp, is_identical_to(gp.tip))
    expect_that(tg$children$node_labels$gp, is_identical_to(gp.node))

    if (interactive()) {
      vp.spacing <- viewport(width=.8, height=.8, name="spacing")
      print(tg, vp=vp.spacing)
    }
  }
})

test_that("Initial angle argument for circle plots", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  set.seed(1)
  theta <- runif(1, 0, 2*pi)
  tg0 <- treeGrob(tr, name="mytree", direction="circle")
  tg1 <- treeGrob(tr, name="mytree", direction="circle", theta0=theta)

  expect_that(tg1$children$branches$spacing_mid,
              equals(tg0$children$branches$spacing_mid + theta))
  expect_that(tg1$children$branches$spacing_min,
              equals(tg0$children$branches$spacing_min + theta))
  expect_that(tg1$children$branches$spacing_max,
              equals(tg0$children$branches$spacing_max + theta))

  if (interactive()) {
    f <- function(theta0) {
      tg <- treeGrob(tr, name="mytree", direction="circle",
                     theta0=theta0) +
                       tree_tip_labels() + tree_node_labels()
      vp <- viewport(width=.8, height=.7, name="spacing")
      print(tg, vp=vp)
    }

    if (FALSE) {
      pdf("plotting-circle-angle.pdf")
      for (i in seq(0, 2*pi, length=50)) {
        f(i)
      }
      dev.off()
    }
  }
})

test_that("Initial angle argument fails for non-circle plot types", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  dirs <- setdiff(forest:::tree_directions(), "circle")
  for (direction in dirs) {
    expect_that(treeGrob(tr, name="t", direction=direction, theta0=0),
                equals(treeGrob(tr, name="t", direction=direction)))
    expect_that(treeGrob(tr, direction=direction, theta0=1),
                throws_error())
  }
})

test_that("Branch styling (returned object)", {
  # Empty style
  sty <- tree_style("foo")
  expect_that(sty, is_a("tree_style"))
  expect_that(names(sty), equals(c("class", "name", "targets", "base",
                                   "descendants")))
  expect_that(sty$class, equals("foo"))
  expect_that(sty$targets, equals(structure(list(), names=character(0))))
  expect_that(sty$base, is_identical_to(gpar()))
  expect_that(sty$name, equals(NULL))
  expect_that(sty$descendants, equals(TRUE))

  # List of targets:
  g0 <- gpar(lwd=2)
  g1 <- gpar(col="red")
  g2 <- gpar(col="blue", lty=3)
  sty <- tree_style("foo", n1=g1, n2=g2, base=g0)
  expect_that(length(sty$targets), equals(2))
  expect_that(names(sty$targets), equals(c("n1", "n2")))
  expect_that(sty$targets$n1, equals(g1))
  expect_that(sty$targets$n2, equals(g2))
  expect_that(sty$base, equals(g0))

  # Unnamed targets:
  expect_that(tree_style("foo", g1, base=g0), throws_error())
  expect_that(tree_style("foo", n1=g1, g2, base=g0), throws_error())
  expect_that(tree_style("foo", n1=g1, n2=g2, g0), throws_error())
  expect_that(tree_style("foo", g0), throws_error())

  # NULL gpars are converted to gpar
  expect_that(tree_style("foo", n1=NULL)$targets$n1, equals(gpar()))
  expect_that(tree_style("foo", n1=gpar())$targets$n1, equals(gpar()))
  expect_that(tree_style("foo", n1=gpar(), n2=g2)$targets$n1, equals(gpar()))

  # Invalid input
  expect_that(tree_style("foo", n1="red"), throws_error())
  expect_that(tree_style("foo", n1=list(col="red")), throws_error())
  expect_that(tree_style("foo", descendants=logical(0)), throws_error())
})

test_that("Branch styling (single regime)", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  tg <- treeGrob(tr) + tree_tip_labels() + tree_node_labels()
  tg2 <- tg + tree_style_branches(n5=gpar(col="red"))

  gp2 <- tg2$children$branches$gp
  expect_that(gp2, is_a("gpar"))
  expect_that(names(gp2), equals("col"))
  expect_that(length(gp2$col), equals(tr$size))
  cl2 <- forest:::classify(tr, "n5")
  cl2 <- cl2[match(tg$children$branches$label, names(cl2))]
  expect_that(gp2$col, equals(c("black", "red")[cl2 + 1L]))

  tg3 <- tg2 + tree_style_tip_labels(n2=gpar(col="blue"))
  gp3 <- tg3$children$tip_labels$gp
  expect_that(gp3, is_a("gpar"))
  expect_that(names(gp3), equals("col"))
  expect_that(length(gp3$col), equals(tr$tips))
  cl3 <- forest:::classify(tr, "n2")
  cl3 <- cl3[match(tg$children$tip_labels$label, names(cl3))]
  expect_that(gp3$col, equals(c("black", "blue")[cl3 + 1L]))

  tg4 <- tg3 + tree_style_node_labels(n4=gpar(col="green4"))
  gp4 <- tg4$children$node_labels$gp
  expect_that(gp4, is_a("gpar"))
  expect_that(names(gp4), equals("col"))
  expect_that(length(gp4$col), equals(tr$nodes))
  cl4 <- forest:::classify(tr, "n4")
  cl4 <- cl4[match(tg$children$node_labels$label, names(cl4))]
  expect_that(gp4$col, equals(c("black", "green4")[cl4 + 1L]))

  if (interactive()) {
    vp <- viewport(width=.8, height=.8, name="spacing")
    print(tg4, vp=vp)
  }
})

## TODO: check can't restyle things (yet)

test_that("Branch styling (corner cases)", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  tg <- treeGrob(tr) + tree_tip_labels() + tree_node_labels()
  tg2 <- tg + tree_style_branches()

  expect_that(tg2$children$branches$gp,
              equals(tg$children$branches$gp))

  gp_base <- gpar(col="red")
  tg3 <- tg + tree_style_branches(base=gp_base)
  expect_that(tg3$children$branches$gp,
              equals(gp_base))

  # TODO: test lower level tree_style?
})

test_that("Branch styling (multiple regimes)", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  tg <- treeGrob(tr) + tree_tip_labels() + tree_node_labels()

  tg2 <- tg + tree_style("tree_branches",
                         n4=gpar(col="blue"),
                         n5=gpar(col="green4", lwd=2),
                         n2=gpar(col="orange"),
                         base=gpar(col="red"))

  cl <- forest:::classify(tr, c("n4", "n5", "n2")) + 1L
  gpp <- forest:::combine_gpars(list(gpar(col="red"), # base
                                     gpar(col="blue"),
                                     gpar(col="green4", lwd=2),
                                     gpar(col="orange")),
                                cl)

  i <- match(tg$children$branches$label, names(cl))

  gp.b <- tg2$children$branches$gp
  expect_that(length(gp.b), equals(2))
  expect_that(names(gp.b), equals(c("col", "lwd")))
  expect_that(gp.b$col, equals(gpp$col[i]))
  expect_that(gp.b$lwd, equals(gpp$lwd[i]))

  if (interactive()) {
    vp <- viewport(width=.8, height=.8, name="spacing")
    print(tg2, vp=vp)
  }
})

test_that("Branch styling (single nodes)", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  tg <- treeGrob(tr) + tree_tip_labels() + tree_node_labels()

  # Lots of repetition and ugliness here, but it's only a test.
  tg2 <- tg + tree_style("tree_labels",
                         n4=gpar(col="blue"),
                         n5=gpar(col="green4", lwd=2),
                         n2=gpar(col="orange"),
                         base=gpar(col="red"),
                         descendants=FALSE)

  lab <- tg2$children$node_labels$label
  idx <- rep(1L, length(lab))
  idx[match(c("n4", "n5", "n2"), lab)] <- 2:4

  gpp <- forest:::combine_gpars(list(gpar(col="red"), # base
                                     gpar(col="blue"),
                                     gpar(col="green4", lwd=2),
                                     gpar(col="orange")),
                                idx)

  gp.n <- tg2$children$node_labels$gp
  expect_that(gp.n$lwd, equals(gpp$lwd))
  expect_that(gp.n$col, equals(gpp$col))

  if (interactive()) {
    vp <- viewport(width=.8, height=.8, name="spacing")
    print(tg2, vp=vp)
  }
})

test_that("tree_image (png)", {
  # Here is an image from within the png package.  It's not very
  # inspiring but it's a start.
  pic.filename <- system.file("img", "Rlogo.png", package="png")
  pic <- readPNG(pic.filename)

  ## Minimal set of options.
  ti <- tree_image(pic, "t1")
  expect_that(ti, is_a("tree_image"))
  expect_that(names(ti),
              equals(c("object", "label", "offset", "rot", "width",
                       "name", "gp")))
  # Check the defaults are as expected.
  expect_that(ti$object, is_a("rastergrob"))
  expect_that(ti$label,  is_identical_to("t1"))
  expect_that(ti$offset, is_a("unit"))
  expect_that(ti$offset, equals(unit(0.5, "lines")))
  expect_that(ti$rot,    is_identical_to(0.0))
  expect_that(ti$width,   equals(unit(1, "native")))

  # Corner cases:
  expect_that(tree_image(),   throws_error())  # picture and label missing
  expect_that(tree_image(pic), throws_error()) # label missing

  # Invalid image
  expect_that(tree_image(NULL, "t1"),         throws_error())
  expect_that(tree_image(pic.filename, "t1"), throws_error())

  # Invalid label
  expect_that(tree_image(NULL, character(0)),  throws_error())
  expect_that(tree_image(NULL, c("t1", "t2")), throws_error())
  # No type checking here though.  And we can't check being in the
  # tree until it joins the tree.

  # Invalid offset: needs to be an unit of length 1
  expect_that(tree_image(pic, "t1", offset=1), throws_error())
  expect_that(tree_image(pic, "t1", offset=unit(1:2, "lines")),
              throws_error())

  # Invalid rotation
  expect_that(tree_image(pic, "t1", rot=c(1, 2)), throws_error())
  expect_that(tree_image(pic, "t1", rot=NA),      throws_error())
  expect_that(tree_image(pic, "t1", rot=NULL),    throws_error())
  expect_that(tree_image(pic, "t1", rot="right"), throws_error())

  # Invalid width
  expect_that(tree_image(pic, "t1", width=1), throws_error())
  expect_that(tree_image(pic, "t1", width=unit(1:2, "lines")),
              throws_error())

  # No validation is done on name or gp, though.

  # Check that options passed through are actually recorded.  This has
  # already been an issue a couple of times.  Might actually be better
  # to build the lists through match.call()?
  label <- "t1"
  offset <- unit(1, "cm")
  rot <- 90
  width <- unit(2, "cm")
  name <- "foo"
  gp <- gpar(lwd=1)
  tmp <- tree_image(pic, label, offset=offset, rot=rot, width=width,
                    name=name, gp=gp)
  expect_that(tmp$label,  is_identical_to(label))
  expect_that(tmp$offset, is_identical_to(offset))
  expect_that(tmp$rot,    is_identical_to(rot))
  expect_that(tmp$width,  is_identical_to(width))
  expect_that(tmp$name,   is_identical_to(name))
  expect_that(tmp$gp,     is_identical_to(gp))

  # Check that native raster images are OK too.
  pic.n <- readPNG(pic.filename, native=TRUE)
  tmp <- tree_image(pic.n, "t1")
  expect_that(tmp$object, is_a("rastergrob"))
})

test_that("tree_image (vector)", {
  pic <- vector_read("files/fish.svg")

  ## Minimal set of options.
  ti <- tree_image(pic, "t1")
  expect_that(ti, is_a("tree_image"))
  expect_that(names(ti),
              equals(c("object", "label", "offset", "rot", "width",
                       "name", "gp")))
  # grImport::pictureGrob differs from other some grob making function
  # in that the grob has the same class as the input, but with grob
  # appended and with the case of the class changed. :/
  expect_that(ti$object, is_a("picture"))
  expect_that(ti$object, is_a("grob"))
})

# This is not really a good test, except that it checks that it checks
# that nothing terrible happens in associating the image and the
# tree.  Work do be done for sure though.
test_that("Add tree_image to a tree", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)
  tg <- treeGrob(tr, direction="right")

  # Same picture as above
  pic.filename <- system.file("img", "Rlogo.png", package="png")
  pic <- readPNG(pic.filename)

  tg2 <- tg + tree_image(pic, "t1", name="myimage",
                         width=unit(1, "cm"))
  expect_that(names(tg2$children), equals(c("branches", "myimage")))

  fish <- vector_read("files/fish.svg")
  tg3 <- tg2 + tree_image(fish, "t4", name="myfish",
                          width=unit(2, "cm"))

  expect_that(names(tg3$children), equals(c("branches", "myimage", "myfish")))

  # TODO:
  # Now, painfully, go through and check that the location is correct?
  # That seems like a lot of work.  Perhaps wait until we have the
  # generalised location information code written.

  if (interactive()) {
    vp.spacing <- viewport(width=.8, height=.8, name="spacing")
    print(tg2, vp=vp.spacing)
    print(tg3, vp=vp.spacing)
  }
})

test_that("tree_braces", {
  # Minimal set of options:
  tb <- tree_braces("t1")
  expect_that(tb,           is_a("tree_braces"))
  expect_that(tb$label,     is_identical_to("t1"))
  expect_that(tb$offset,    is_a("unit"))
  expect_that(tb$offset,    equals(unit(0.5, "lines")))
  expect_that(tb$alignment, is_identical_to("none"))
  expect_that(tb$name,      is_identical_to(NULL))
  expect_that(tb$gp,        is_identical_to(gpar()))

  # Corner cases:
  expect_that(tree_braces(), throws_error())  # label missing

  # Invalid label
  expect_that(tree_braces(character(0)),  throws_error())
  # No type checking here though.  And we can't check being in the
  # tree until it joins the tree.

  # Invalid offset: needs to be an unit of length 1
  expect_that(tree_braces("t1", offset=1),                  throws_error())
  expect_that(tree_braces("t1", offset=unit(1:2, "lines")), throws_error())

  expect_that(tree_braces("t1", alignment=NA),        throws_error())
  expect_that(tree_braces("t1", alignment="invalid"), throws_error())

  # No validation is done on name or gp, though.

  # Check that options passed through are actually recorded.  This has
  # already been an issue a couple of times.  Might actually be better
  # to build the lists through match.call()?
  label <- c("t1", "t2")
  offset <- unit(1, "cm")
  alignment <- "global"
  name <- "foo"
  gp <- gpar(lwd=1)
  tmp <- tree_braces(label, offset=offset, alignment=alignment,
                     name=name, gp=gp)

  expect_that(tmp$label,     is_identical_to(label))
  expect_that(tmp$offset,    is_identical_to(offset))
  expect_that(tmp$alignment, is_identical_to(alignment))
  expect_that(tmp$name,      is_identical_to(name))
  expect_that(tmp$gp,        is_identical_to(gp))
})

test_that("Add tree_braces to a tree", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  for (direction in forest:::tree_directions()) {
    tg <- treeGrob(tr, direction=direction) + tree_node_labels()

    tb <- tree_braces("n4", name="brace")
    tg2 <- tg + tb

    expect_that(names(tg2$children),
                equals(c("branches", "node_labels", "brace")))
    expect_that(tg2$children$brace, is_a("tree_braces"))

    # TODO: Check that location is correct.

    if (interactive()) {
      vp.spacing <- viewport(width=.8, height=.8, name="spacing")
      print(tg2, vp=vp.spacing)
    }
  }
})

test_that("More than one brace", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  for (direction in forest:::tree_directions()) {
    tg <- treeGrob(tr, direction=direction) + tree_node_labels()

    at <- c("n8", "n5", "n2")
    tb <- tree_braces(at, name="brace")
    tg2 <- tg + tb

    expect_that(names(tg2$children),
                equals(c("branches", "node_labels", "brace")))
    expect_that(tg2$children$brace, is_a("tree_braces"))
    expect_that(tg2$children$brace$label, is_identical_to(at))

    # TODO: Check that location is correct.

    if (interactive()) {
      vp.spacing <- viewport(width=.8, height=.8, name="spacing")
      print(tg2, vp=vp.spacing)
    }
  }
})

test_that("Brace alignment", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  vp.spacing <- viewport(width=.8, height=.8, name="spacing")
  tg <- treeGrob(tr, direction="right", vp=vp.spacing) + tree_node_labels()

  at <- c("n8", "n5")

  tg.n <- tg + tree_braces(at, name="brace", alignment="none")
  tg.s <- tg + tree_braces(at, name="brace", alignment="set")
  tg.g <- tg + tree_braces(at, name="brace", alignment="global")

  # Check the time position of these three different alignments:
  spp <- sapply(at, function(nd) tr$get_subtree(nd)$tip_labels)
  br <- tg$children$branches
  at.t <- sapply(spp, function(x)
                 max(br$time_tipward[match(x, br$label)]))

  offset <- tree_braces(at, name="brace")$offset

  expect_that(unit(at.t, "native") + offset,
              equals(tg.n$children$brace$t))
  expect_that(unit(rep(max(at.t), length(at)), "native") + offset,
              equals(tg.s$children$brace$t))
  expect_that(unit(rep(max(br$time_tipward), length(at)), "native") +
              offset,
              equals(tg.g$children$brace$t))

  if (interactive()) {
    print(tg.n)
    print(tg.s)
    print(tg.g)
  }
})

test_that("brace_style", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  vp.spacing <- viewport(width=.8, height=.8, name="spacing")
  tg <- treeGrob(tr, direction="right", vp=vp.spacing) +
    tree_node_labels() + tree_braces(c("n2", "n8", "n5"), name="brace")

  # One colour for all -- this might want to be easier in general,
  # actually.  Perhaps I'm being over-eager in assuming
  # phylogenetically distributed styles?
  gp <- gpar(col="red", lwd=2)
  tg2 <- tg + tree_style_brace(base=gp)
  expect_that(tg2$children$brace$gp, equals(gp))

  # Now, with varying styles:
  tg3 <- tg + tree_style_brace(n8=gpar(col="red"),
                               n2=gpar(col="blue"),
                               n5=gpar(col="green4"))
  cl <- forest:::classify(tr, c("n8", "n2", "n5")) + 1L
  gpp <- forest:::combine_gpars(list(gpar(),
                                     gpar(col="red"),
                                     gpar(col="blue"),
                                     gpar(col="green4")),
                                cl[tg3$children$brace$label])
  expect_that(tg3$children$brace$gp, equals(gpp))

  # Now inherited style; n4 matches the nodes n8 and n5
  tg4 <- tg + tree_style_brace(n4=gpar(col="blue"),
                               base=gp)

  cl <- forest:::classify(tr, c("n4")) + 1L
  gpp <- forest:::combine_gpars(list(gp, gpar(col="blue")),
                                cl[tg4$children$brace$label])
  expect_that(tg4$children$brace$gp, equals(gpp))

  # Turning descendents off, n4 now matches nothing:
  tg5 <- tg + tree_style_brace(n4=gpar(col="blue"),
                               base=gp, descendants=FALSE)
  expect_that(tg5$children$brace$gp,
              equals(do.call(gpar, lapply(gp, rep, 3))))

  if (interactive()) {
    print(tg2)
    print(tg3)
    print(tg4)
    print(tg5)
  }
})

test_that("tree_match", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  phy$tip.label <- paste0(phy$tip.label, "abcde")
  tr <- forest.from.ape(phy)

  tg <- treeGrob(tr) +
    tree_tip_labels() + tree_node_labels() + tree_braces("n4")

  # Need to specify at least one of class / name
  expect_that(tree_match(tg),             throws_error())
  expect_that(tree_match(tg, NULL, NULL), throws_error())

  # Not in the tree - return empty list and give a warning.
  expect_that(tmp <- tree_match(tg, class="nope"),
              gives_warning())
  expect_that(tmp, equals(list()))
  expect_that(tmp <- tree_match(tg, name="nope"),
              gives_warning())
  expect_that(tmp, equals(list()))
  expect_that(tmp <- tree_match(tg, class="nope", name="nope"),
              gives_warning())
  expect_that(tmp, equals(list()))

  # Warning can be turned off:
  expect_that(tree_match(tg, class="nope", warn_no_match=FALSE),
              not(gives_warning()))
  expect_that(tree_match(tg, name="nope", warn_no_match=FALSE),
              not(gives_warning()))
  expect_that(tree_match(tg, class="name", name="nope", warn_no_match=FALSE),
              not(gives_warning()))

  # Match on class:
  expect_that(tree_match(tg, class="tree_branches"),
              equals(list(gPath("branches"))))

  # More than one instance of tree_labels:
  expect_that(tree_match(tg, class="tree_labels"),
              equals(list(gPath("tip_labels"), gPath("node_labels"))))

  # tree_braces had no name so is a generated name.  This is basically
  # the reson for the existance tree_match:
  cmp <- gPath(names(which(sapply(tg$children, inherits, "tree_braces"))))
  expect_that(tree_match(tg, class="tree_braces"),
              equals(list(cmp)))

  # Match on name:
  expect_that(tree_match(tg, name="tip_labels"),
              equals(list(gPath("tip_labels"))))

  # Match on nonexistant name for class that does exist:
  expect_that(tree_match(tg, class="tree_labels", name="nope",
                         warn_no_match=FALSE),
              equals(list()))

  # Currently no support for abbreviated names:
  expect_that(tree_match(tg, class="label", warn_no_match=FALSE),
              equals(list()))

  # But there *is* unfortunate support for matching grobs:
  expect_that(tree_match(tg, class="grob"),
              equals(lapply(names(tg$children), gPath)))
})


## TODO: classify on root note?
