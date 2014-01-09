source("helper-forest.R")

context("General tree")

tree_of <- make.tree_of(xtree)

# A simple small tree with no branch lengths, and with labels changed
# to be their ape indices:
library(ape)
set.seed(1)
phy <- rtree(5)
phy$edge.length <- NULL
phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

# Make a node and also extract information from the tree for labels:
x <- function(i)
  new(xnode, i, c(phy$tip.label, phy$node.label)[[i]])
cmp <- tree_of(x(6))(tree_of(x(7))(x(1), x(2)),
                     tree_of(x(8))(x(3), tree_of(x(9))(x(4), x(5))))()

## Build a tree from ape; these are basically the same as the integer
## versions (in test-itree.R)
from.ape.recursive <- function(phy) {
  from <- phy$edge[,1]
  to   <- phy$edge[,2]
  n    <- phy$Nnode + Ntip(phy)
  sub  <- vector("list", n)
  desc <- split(to, factor(from, seq_len(n)))

  lab <- c(phy$tip.label, phy$node.label)

  f <- function(nd) {
    tr <- new(xtree, new(xnode, nd, lab[[nd]]))
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

  lab <- c(phy$tip.label, phy$node.label)

  for (i in c(seq_len(Ntip(phy)), unique(from))) {
    sub.i <- new(xtree, new(xnode, i, lab[[i]]))
    for (x in sub[desc[[i]]])
      sub.i$append_subtree(x)
    sub[[i]] <- sub.i
  }

  sub[[i]]
}

test_that("Conversion from ape to forest works", {
  tr.r <- from.ape.recursive(phy)
  tr.i <- from.ape.iterative(phy)

  expect_that(tr.r$equals(cmp), is_true())
  expect_that(tr.i$equals(cmp), is_true())
})

## This is set up to return things in the same order as an ape
## pruningwise edge matrix.  This pretty much only works if a tree was
## orignally an ape tree to make the numbers work correctly.  It also
## requires that the index be unique.  Both of these things are
## complete hassles, but this is not a huge priority to fix right now.
to.ape <- function(tr) {
  cbind0 <- function(...) cbind(..., deparse.level=0)
  edge <- matrix(nrow=0, ncol=2)
  n.node <- n.tip <- 0

  label  <- rep.int(NA_character_, tr$size)
  is.tip <- rep.int(NA,            tr$size)

  i <- 1L
  from <- tr$begin_sub_post()
  to   <- tr$end_sub_post()
  while (from$differs(to)) {
    ## TODO: from$value$front() should work but crashes.
    nd <- from$value$begin()$value
    label[[i]]    <- nd$label
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

  structure(list(edge=edge,
                 tip.label=label[is.tip],
                 Nnode=sum(!is.tip),
                 node.label=label[!is.tip]),
            class="phylo", order="pruningwise")
}

test_that("Conversion from forest to ape works", {
  expect_that(to.ape(cmp), equals(phy))
})

## Here is a newick string writer, based on the lexical_cast code.
## It's recursive, but I think that an iterative pre-order traversal
## version might be possible too; the big issue is knowing when the
## jumps take place.  That might be possible with a combination of
## pre_sub and child -- basicaly what we used in to.ape.
to.newick <- function(tr) {
  f <- function(tr) {
    if (tr$childless) {
      tr$root()$label
    } else {
      str <- "("
      i1 <- tr$begin_sub_child()
      i2 <- tr$end_sub_child()
      while (i1$differs(i2)) {
        str <- c(str, f(i1$post_increment()$value))
        if (i1$differs(i2))
          str <- c(str, ",")
      }
      str <- c(str, ")", tr$root()$label)
      paste(str, collapse="")
    }
  }
  paste0(f(tr), ";")
}

test_that("Conversion from forest to newick works", {
  expect_that(to.newick(cmp),
              is_identical_to(write.tree(phy)))
})
