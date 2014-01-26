source("helper-forest.R")

context("Ape conversion")

test_that("Simple ape -> forest conversion works", {
  # Start with a small tree that we can write out easily (5 tips).
  set.seed(1)
  phy <- ape::rtree(5)
  phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

  # Version with no edge lengths:
  phy0 <- phy
  phy0$edge.length <- NULL

  # These are Newick representations of the trees above, given the
  # seed above and assuming that ape does not change the generator.
  str <- "((t3:0.6291140439,t4:0.06178627047)n7:0.6607977925,(t1:0.1765567525,(t2:0.3841037182,t5:0.76984142)n9:0.6870228467)n8:0.2059745749)n6;"
  str0 <- "((t3,t4)n7,(t1,(t2,t5)n9)n8)n6;"

  cmp <- from.newick.string(str)
  cmp0 <- from.newick.string(str0)

  ## Also get the "data"
  cmp.data <- match(unlist(treeapply(cmp, function(x) x$label)),
                    c(phy$tip.label, phy$node.label))
  cmp.data <- as.list(as.numeric(cmp.data))

  tr <- forest.from.ape(phy)
  tr0 <- forest.from.ape(phy0)

  # Here is the RObject/SEXP comparison issue again:
  #   expect_that(tr$equals(cmp), is_true())
  # I'm going to have to deal with that eventually...
  expect_that(tr$representation, is_identical_to(cmp$representation))
  expect_that(to.newick.string(tr),
              is_identical_to(to.newick.string(cmp)))
  expect_that(treeapply(tr, function(x) x$data),
              is_identical_to(cmp.data))

  expect_that(tr0$representation, is_identical_to(cmp0$representation))
  expect_that(to.newick.string(tr0),
              is_identical_to(to.newick.string(cmp0)))
  expect_that(treeapply(tr0, function(x) x$data),
              is_identical_to(cmp.data))

  # And back the other way:
  conv  <- ape.from.forest(tr)
  conv0 <- ape.from.forest(tr0)

  # Be aware the this might not check node labels correctly..
  expect_that(conv,  equals(phy))
  expect_that(conv0, equals(phy0))
})
