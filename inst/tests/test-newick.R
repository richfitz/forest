source("helper-forest.R")

context("Read Newick tree")

## Parse the node part of a Newick string:
test_that("Parse node with no length", {
  label <- "foo"
  nd <- forest:::from_newick_node(label)
  expect_that(nd$label, is_identical_to(label))
  expect_that(nd$has_length, is_false())
  expect_that(nd$length, is_identical_to(NA_real_))
})

test_that("Parse node with branch length", {
  label <- "foo"
  length <- pi
  nd <- forest:::from_newick_node(paste(label, length, sep=":"))
  expect_that(nd$label, is_identical_to(label))
  expect_that(nd$has_length, is_true())
  expect_that(nd$length, equals(length))
})

test_that("Don't parse node with invalid branch length", {
  expect_that(forest:::from_newick_node("label:invalid_length"),
              throws_error())
})

## This probably falls outside of the Newick standard, but documenting
## that I *will* accept strings with embedded colons -- we'll take the
## bit after the final colon as the branch length.
test_that("Parse node with branch length and embedded colon", {
  label <- "foo:bar"
  length <- pi
  nd <- forest:::from_newick_node(paste(label, length, sep=":"))
  expect_that(nd$label, is_identical_to(label))
  expect_that(nd$has_length, is_true())
  expect_that(nd$length, equals(length))
})

test_that("Read easy tree, no branch lengths", {
  str <- "(a,(b,c)i)r;"
  tr <- from.newick.string(str)
  expect_that(str, is_identical_to(to.newick.string(tr)))
})

test_that("Read tree with no node labels", {
  str <- "(a,(b,c));"
  tr <- from.newick.string(str)
  expect_that(str, is_identical_to(to.newick.string(tr)))
})

test_that("Read tree with edge length", {
  str <- "(a:1.352452,(b,c));"
  tr <- from.newick.string(str)
  expect_that(str, is_identical_to(to.newick.string(tr)))
})

test_that("Read ape::rtree() tree", {
  set.seed(1)
  phy <- rtree(10)
  str <- write.tree(phy)

  tr <- from.newick.string(str)
  expect_that(str, is_identical_to(to.newick.string(tr)))
})

# This won't get run unless there is a directory "harmon-2010-trees"
# containing the trees from Harmon et al 2010 (early bursts are
# rare...).  To set this up, run
#   get.harmon.trees("harmon-2010-trees")
test_that("Harmon trees can be read in", {
  harmon.path <- "harmon-2010-trees"
  if (file.exists(harmon.path)) {
    files <- dir(harmon.path, pattern="phy$", full.names=TRUE)

    ## Read in all trees, ignoring issues with newlines
    str <- suppressWarnings(lapply(files, readLines))

    ## Read:  0.5s
    ## Write: 0.32s
    phy.ape    <- lapply(files, ape::read.tree)
    newick.ape <- lapply(phy.ape, write.tree)

    ## Read:  0.2s
    ## Write: 0.01s
    phy <- lapply(str, from.newick.string)
    system.time(newick <- lapply(phy, to.newick.string))

    expect_that(newick, is_identical_to(newick.ape))
  }
})
