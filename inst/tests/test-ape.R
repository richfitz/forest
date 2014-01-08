source("helper-forest.R")

context("Ape tree import/export")

library(ape)

## A simple small tree with no branch lengths, and with labels changed
## to be their ape indices:
set.seed(1)
phy <- rtree(5)
phy$edge.length <- NULL
phy$tip.label <- seq_along(phy$tip.label)
phy$node.label <- seq_len(phy$Nnode) + Ntip(phy)

test_that("ape generated tree is what I expected", {
  ## Check the tree in newick is what I expected it to be (protection
  ## from upstream ape changes):
  expect_that(write.tree(phy),
              equals("((1,2)7,(3,(4,5)9)8)6;"))
})

## Here is the tree that we're aiming for, in forest itree format:
cmp <- tree_of(6)(tree_of(7)(1, 2),
                  tree_of(8)(3, tree_of(9)(4, 5)))()

## Conversion from ape to forest itree:

## Not trying to do these particularly efficiently.  For now, focussing
## only on the indices, but everything after that is easy because we
## can index names using indicies with
##   all.names <- c(phy$tip.label, phy$node.label)
## and set them up on the new(itree, nd) line.
##
## Similarly for edge lengths:
##   idx <- seq_len(phy$Nnode + Ntip(phy))
##   all.len <- phy$edge.length[match(idx, phy$edge[,2])]
##
## Ideally this will move into C at some point.

## There are two versions -- the recursive one might overflow the
## stack, and the iterative one has potentially ugly memory
## properties.  If the iterative one could be rewritten to use splice,
## then it would *move* the tree and work in O(N) space.
from.ape.recursive <- function(phy) {
  from <- phy$edge[,1]
  to   <- phy$edge[,2]
  n    <- phy$Nnode + Ntip(phy)
  sub  <- vector("list", n)
  desc <- split(to, factor(from, seq_len(n)))

  f <- function(nd) {
    tr <- new(itree, nd)
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

  for (i in c(seq_len(Ntip(phy)), unique(from))) {
    sub.i <- new(itree, i)
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
## pruningwise edge matrix.  It will be simplified when we have tip
## and node counters -- oddly they are missing at the moment in
## tree.hpp!
to.ape <- function(tr) {
  cbind0 <- function(...) cbind(..., deparse.level=0)
  from <- tr$begin_sub_post()
  to   <- tr$end_sub_post()
  edge <- matrix(nrow=0, ncol=2)
  n.node <- n.tip <- 0
  while (from$differs(to)) {
    if (from$value$childless) {
      n.tip <- n.tip + 1
    } else {
      n.node <- n.node + 1
      c1 <- from$value$begin_child()
      cn <- from$value$end_child()
      dest <- integer(0)
      while (c1$differs(cn))
        dest <- c(dest, c1$post_increment()$value)
      edge <- rbind(edge, cbind0(from$value$begin()$value, dest))
    }
    from$increment()
  }

  structure(list(edge=edge,
                 tip.label=seq_len(n.tip),
                 Nnode=n.node,
                 node.label=seq_len(n.node)),
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
      tr$root()
    } else {
      str <- "("
      i1 <- tr$begin_sub_child()
      i2 <- tr$end_sub_child()
      while (i1$differs(i2)) {
        str <- c(str, f(i1$post_increment()$value))
        if (i1$differs(i2))
          str <- c(str, ",")
      }
      str <- c(str, ")", tr$root())
      paste(str, collapse="")
    }
  }
  paste0(f(tr), ";")
}

test_that("Conversion from forest to newick works", {
  expect_that(to.newick(cmp),
              is_identical_to(write.tree(phy)))
})
