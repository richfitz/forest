source("helper-forest.R")

## New tests because this is getting silly.


## A couple of little ape trees for testing
set.seed(1)
phy <- ape::rtree(5)
phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

# Version with no edge lengths:
phy0 <- phy
phy0$edge.length <- NULL

tr <- forest_tree(phy)
tr0 <- forest_tree(phy0)

context("forest_tree")

## Manual construction.  Not actually that common I think.
test_that("Empty tree", {
  tr <- forest_tree()
  expect_that(tr$is_empty(),       is_true())
  expect_that(tr$size(),           equals(0))
  expect_that(tr$arity(),          equals(0))
  expect_that(tr$childless(),      is_true())
  expect_that(tr$representation(), equals(""))

  expect_that(tr$count_tips(),     equals(0))
  expect_that(tr$count_nodes(),    equals(0))
})

test_that("Construct with root node", {
  nd <- forest_node("root", pi, list(1, 2))
  tr <- forest_tree(nd)
  expect_that(tr$is_empty(),       is_false())
  expect_that(tr$size(),           equals(1))
  expect_that(tr$arity(),          equals(0))
  expect_that(tr$childless(),      is_true())
  expect_that(tr$representation(), equals("root"))

  expect_that(tr$count_tips(),      equals(1))
  expect_that(tr$count_nodes(),     equals(0)) # NOTE: sensible?

  nd2 <- tr$root_node()
  expect_that(nd2, is_identical_to(nd))
  nd2$length <- 0
  ## Once the node has been copied out of the tree it does *not*
  ## remain attached to the tree -- changes are not propagated at
  ## all.
  expect_that(nd$length, equals(pi))
  expect_that(tr$root_node()$length, equals(pi))
  expect_that(tr$tip_labels(), equals(nd$label))

  tr2 <- tr$copy_structure()
  expect_that(tr2$root_node()$label,  is_identical_to(tr$root_node()$label))
  expect_that(tr2$root_node()$length, is_identical_to(tr$root_node()$length))
  expect_that(tr2$root_node()$data,   equals(NULL))
})

test_that("tips and nodes works", {
  expect_that(tr$count_tips(),  equals(Ntip(phy)))
  expect_that(tr$count_nodes(), equals(phy$Nnode))
  expect_that(tr$tip_labels(),  equals(phy$tip.label))
  expect_that(tr$node_labels(), equals(phy$node.label))
})

## Phylogenetic stuff:
## is_binary
## has_branch_lengths (check if assigning a single NA length, and on
##   ill-formed trees)
## height calculations
