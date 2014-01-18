source("helper-forest.R")

context("Ape conversion")

test_that("Simple ape -> forest conversion works", {
  # Start with a small tree that we can write out easily (5 tips).
  set.seed(1)
  phy <- rtree(5)
  phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

  # Version with no edge lengths:
  phy0 <- phy
  phy0$edge.length <- NULL

  # These are manually constructed "correct" conversions (given the seed
  # above and assuming that ape does not change the generator).
  tree_of <- make.tree_of(xtree)
  x <- make.node.builder.xnode(phy)
  cmp <- tree_of(x(6))(tree_of(x(7))(x(1), x(2)),
                       tree_of(x(8))(x(3), tree_of(x(9))(x(4), x(5))))()

  x <- make.node.builder.xnode(phy0)
  cmp0 <- tree_of(x(6))(tree_of(x(7))(x(1), x(2)),
                        tree_of(x(8))(x(3), tree_of(x(9))(x(4), x(5))))()

  tr <- forest.from.ape(phy)
  tr0 <- forest.from.ape(phy0)

  # Here is the RObject/SEXP comparison issue again:
  #   expect_that(tr$equals(cmp), is_true())
  # I'm going to have to deal with that eventually...
  expect_that(tr$representation, is_identical_to(cmp$representation))
  expect_that(to.newick.string(tr),
              is_identical_to(to.newick.string(cmp)))
  expect_that(treeapply(tr, function(x) x$data),
              is_identical_to(treeapply(cmp, function(x) x$data)))

  expect_that(tr0$representation, is_identical_to(cmp0$representation))
  expect_that(to.newick.string(tr0),
              is_identical_to(to.newick.string(cmp0)))
  expect_that(treeapply(tr0, function(x) x$data),
              is_identical_to(treeapply(cmp0, function(x) x$data)))

  # And back the other way:
  conv  <- ape.from.forest(tr)
  conv0 <- ape.from.forest(tr0)

  # Be aware the this might not check node labels correctly..
  expect_that(conv,  equals(phy))
  expect_that(conv0, equals(phy0))
})
