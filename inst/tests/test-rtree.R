source("helper-forest.R")

# Most of the things here will be tested more thoroughly in the main
# tree testing (test-tree and test-itree).  This file will change
# considerably during the reorganisation).

context("Simple tree")

rtree <- forest:::rtree

test_that("Empty tree", {
  tr <- new(rtree)
  expect_that(tr$empty,          is_true())
  expect_that(tr$size,           equals(0))
  expect_that(tr$arity,          equals(0))
  expect_that(tr$childless,      is_true())
  expect_that(tr$representation, equals(""))

  expect_that(tr$tips,           equals(0))
  expect_that(tr$nodes,          equals(0))
})

test_that("Construct with root node", {
  nd <- new(rnode, "root", pi, list(1, 2))
  tr <- new(rtree, nd)
  expect_that(tr$empty,          is_false())
  expect_that(tr$size,           equals(1))
  expect_that(tr$arity,          equals(0))
  expect_that(tr$childless,      is_true())
  expect_that(tr$representation, equals("root"))

  expect_that(tr$tips,           equals(1))
  expect_that(tr$nodes,          equals(0)) # NOTE: sensible?

  nd2 <- tr$root_node
  expect_that(nd2$equals(nd), is_true())
  nd2$length <- 0
  # Does not flow through at all:
  expect_that(nd$length, equals(pi))
  expect_that(tr$root_node$length, equals(pi))
  expect_that(tr$tip_labels, equals(nd$label))

  expect_that(tr$is_binary, throws_error()) # not defined

  tr2 <- tr$duplicate_topology()
  expect_that(tr2$root_node$label,  is_identical_to(tr$root_node$label))
  expect_that(tr2$root_node$length, is_identical_to(tr$root_node$length))
  expect_that(tr2$root_node$data,   equals(NULL))
})
