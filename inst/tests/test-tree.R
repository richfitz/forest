source("helper-forest.R")

context("General tree")

tree_of <- make.tree_of(xtree)

set.seed(1)
phy <- rtree(5)
phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

# Version with no edge lengths:
phy0 <- phy
phy0$edge.length <- NULL

# Make a node and also extract information from the tree for labels:
make.node.builder <- function(phy) {
  label  <- c(phy$tip.label, phy$node.label)
  idx    <- seq_len(phy$Nnode + Ntip(phy))
  length <- phy$edge.length[match(idx, phy$edge[,2])]
  if (is.null(length))
    length <- rep(NA_real_, length(label))
  function(i)
    new(xnode, label[[i]], length[[i]], as.integer(i))
}

tree_of <- make.tree_of(xtree)

x <- make.node.builder(phy)
cmp <- tree_of(x(6))(tree_of(x(7))(x(1), x(2)),
                     tree_of(x(8))(x(3), tree_of(x(9))(x(4), x(5))))()

x <- make.node.builder(phy0)
cmp0 <- tree_of(x(6))(tree_of(x(7))(x(1), x(2)),
                      tree_of(x(8))(x(3), tree_of(x(9))(x(4), x(5))))()

## Build a tree from ape; these are basically the same as the integer
## versions (in test-itree.R)
from.ape.recursive <- function(phy) {
  from <- phy$edge[,1]
  to   <- phy$edge[,2]
  n    <- phy$Nnode + Ntip(phy)
  sub  <- vector("list", n)
  desc <- split(to, factor(from, seq_len(n)))

  label  <- c(phy$tip.label, phy$node.label)
  length <- phy$edge.length[match(seq_len(n), phy$edge[,2])]
  if (is.null(length))
    length <- rep.int(NA_real_, n)

  f <- function(nd) {
    tr <- new(xtree, new(xnode, label[[nd]], length[[nd]], as.integer(nd)))
    for (i in desc[[nd]])
      tr$append_subtree(f(i))
    tr
  }
  f(Ntip(phy) + 1)
}

from.ape.iterative <- function(phy) {
  phy <- reorder(phy, "pruningwise")
  from <- phy$edge[,1]
  to   <- phy$edge[,2]
  n    <- phy$Nnode + Ntip(phy)
  sub  <- vector("list", n)
  desc <- split(to, factor(from, seq_len(n)))

  label  <- c(phy$tip.label, phy$node.label)
  length <- phy$edge.length[match(seq_len(n), phy$edge[,2])]
  if (is.null(length))
    length <- rep.int(NA_real_, n)

  for (i in c(seq_len(Ntip(phy)), unique(from))) {
    sub.i <- new(xtree, new(xnode, label[[i]], length[[i]], as.integer(i)))
    for (x in sub[desc[[i]]])
      sub.i$append_subtree(x)
    sub[[i]] <- sub.i
  }

  sub[[i]]
}

test_that("Conversion from ape to forest works (with edge lengths)", {
  tr.r <- from.ape.recursive(phy)
  tr.i <- from.ape.iterative(phy)

  ## See "without edge lengths", above.
  ## expect_that(tr.r$equals(cmp), is_true())
  ## expect_that(tr.i$equals(cmp), is_true())
  expect_that(tr.r$representation, is_identical_to(cmp0$representation))
  expect_that(tr.i$representation, is_identical_to(cmp0$representation))
})

test_that("Conversion from ape to forest works (without edge lengths)", {
  tr.r <- from.ape.recursive(phy0)
  tr.i <- from.ape.iterative(phy0)

  ## TODO: This should work:
  ##   expect_that(tr.i$equals(cmp0), is_true())
  ## But there seems to be issues with the == operator on
  ## Rcpp::RObjects; I don't get equality where I'd expect to.  More
  ## worrying is that only a couple of cases flash up as different --
  ## in particular the node test case does work OK.  It's possible
  ## that a tree of RObject is not being well behaved...
  expect_that(tr.r$representation, is_identical_to(cmp0$representation))
  expect_that(tr.i$representation, is_identical_to(cmp0$representation))
})

## This is set up to return things in the same order as an ape
## pruningwise edge matrix.  This pretty much only works if a tree was
## orignally an ape tree to make the numbers work correctly.  It also
## requires that the index be unique.  Both of these things are
## complete hassles, but this is not a huge priority to fix right now.
##
## Actualy: node labels aren't correctly assigned either, so this is
## broken for both with-and-without edge length cases:
to.ape <- function(tr) {
  cbind0 <- function(...) cbind(..., deparse.level=0)
  edge <- matrix(nrow=0, ncol=2)
  n.node <- n.tip <- 0

  label  <- rep.int(NA_character_, tr$size)
  length <- rep.int(NA_real_,      tr$size)
  is.tip <- rep.int(NA,            tr$size)

  i <- 1L
  from <- tr$begin_sub_post()
  to   <- tr$end_sub_post()
  while (from$differs(to)) {
    ## TODO: from$value$front() should work but crashes.
    nd <- from$value$begin()$value
    label[[i]]  <- nd$label
    length[[i]] <- nd$length
    is.tip[[i]] <- from$value$childless

    if (!from$value$childless) {
      c1 <- from$value$begin_child()
      cn <- from$value$end_child()
      dest <- integer(0)
      while (c1$differs(cn))
        dest <- c(dest, c1$post_increment()$value$data)
      edge <- rbind(edge, cbind0(from$value$begin()$value$data, dest))
    }
    from$increment()
    i <- i + 1L
  }

  edge.length <- if (all(is.na(length))) NULL else length[edge[,2]]

  structure(list(edge=edge,
                 tip.label=label[is.tip],
                 edge.length=edge.length,
                 Nnode=sum(!is.tip),
                 node.label=label[!is.tip]),
            class="phylo", order="pruningwise")
}

test_that("Conversion from forest to ape works", {
  # expect_that(to.ape(cmp),  equals(phy)) # not working yet
  expect_that(to.ape(cmp0), equals(phy0))

  # These all confirm work to do:
  expect_that(isTRUE(all.equal(unclass(to.ape(cmp)), unclass(phy))),
              is_false())
  expect_that(isTRUE(all.equal(unclass(to.ape(cmp0)), unclass(phy0))),
              is_false())
  expect_that(isTRUE(all.equal(unclass(to.ape(cmp)), unclass(phy))),
              is_false())
})

to.newick <- function(tr, digits=10) {
  fmt.digits <- paste0("%.", digits, "g")
  fmt <- function(nd) {
    len <- nd$length
    if (is.na(len))
      nd$label
    else
      paste(nd$label, sprintf(fmt.digits, len), sep=":")
  }

  f <- function(tr) {
    if (tr$childless) {
      fmt(tr$root())
    } else {
      str <- "("
      i1 <- tr$begin_sub_child()
      i2 <- tr$end_sub_child()
      while (i1$differs(i2)) {
        str <- c(str, f(i1$post_increment()$value))
        if (i1$differs(i2))
          str <- c(str, ",")
      }
      str <- c(str, ")", fmt(tr$root()))
      paste(str, collapse="")
    }
  }
  paste0(f(tr), ";")
}

test_that("Conversion from forest to newick works", {
  expect_that(to.newick(cmp),
              is_identical_to(write.tree(phy)))
  expect_that(to.newick(cmp0),
              is_identical_to(write.tree(phy0)))
})

test_that("tips and nodes works", {
  expect_that(cmp$tips,        equals(Ntip(phy)))
  expect_that(cmp$nodes,       equals(phy$Nnode))
  expect_that(cmp$tip_labels,  equals(phy$tip.label))
  expect_that(cmp$node_labels, equals(phy$node.label))

  expect_that(cmp[[1]]$tips,        equals(2))
  expect_that(cmp[[1]]$nodes,       equals(1))
  expect_that(cmp[[1]]$tip_labels,  equals(c("t3", "t4")))
  expect_that(cmp[[1]]$node_labels, equals("n7"))
})

test_that("Height calculation", {
  ## This is a hack to generate a reasonable length tree:
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- from.newick.string(write.tree(phy))

  tr$update_heights()
  heights <- unlist(treeapply(tr, function(nd)
                              structure(nd$height, names=nd$label)))
  depths <- unlist(treeapply(tr, function(nd)
                              structure(nd$depth, names=nd$label)))

  heights.cmp <- sort(diversitree:::branching.heights(phy))
  depths.cmp <- max(heights.cmp) - heights.cmp

  expect_that(heights[names(heights.cmp)], equals(heights.cmp))
  expect_that(depths[names(depths.cmp)],   equals(depths.cmp))

  expect_that(tr$heights, is_identical_to(unname(heights)))
  expect_that(tr$depths,  is_identical_to(unname(depths)))
})
