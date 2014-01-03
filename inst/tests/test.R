source("helper-forest.R")

# These tests will slowly duplicate those in
# src/treetree/test_runner.cpp

context("Basic tree operations")

test_that("Tree built with empty constructor is empty", {
  tr <- new(itree)
  expect_that(tr$empty, is_true())
  expect_that(tr$size, equals(0))
  expect_that(tr$childless, is_true())
  expect_that(tr$representation, is_identical_to(""))
  expect_that(tr$index, throws_error())

  ## Additional to test_runner:
  expect_that(tr$indices, equals(integer(0)))
})

test_that("Tree with root only is valid", {
  tr <- new(itree, 42)
  expect_that(tr$empty, is_false())
  expect_that(tr$size, equals(1))
  expect_that(tr$childless, is_true())
  expect_that(tr$representation, is_identical_to("42"))
  expect_that(tr$index, equals(0))

  ## Additional to test_runner:
  expect_that(tr$indices, equals(0))
})

is_expected_tree <- function(n, representation) {
  function(tr) {
    ok <- (isTRUE(all.equal(tr$size, n))           &&
           isTRUE(all.equal(tr$empty, n == 0))     &&
           isTRUE(all.equal(tr$childless, n <= 1)) &&
           isTRUE(all.equal(tr$representation, representation)))
    expectation(ok, "Tree does not have expected contents")
  }
}

test_that("Can construct a tree via insertion", {
  tr <- new(itree)
  expect_that(tr, is_expected_tree(0, ""))
  expect_that(tr$arity, equals(0))

  tr$insert_root(1) # really is append()?
  expect_that(tr, is_expected_tree(1, "1"))
  expect_that(tr$arity, equals(0))

  tr$insert_at_node(0, 2)
  expect_that(tr, is_expected_tree(2, "1(2)"))
  expect_that(tr$arity, equals(1))

  tr$insert_at_node(0, 3)
  expect_that(tr, is_expected_tree(3, "1(2 3)"))
  expect_that(tr$arity, equals(2))  

  expect_that(tr$indices, equals(0:2))
  
  tr$insert_at_node(1, 4) # looks like node '2'
  expect_that(tr, is_expected_tree(4, "1(2(4) 3)"))

  tr$insert_at_node(1, 5) # looks like node '2'
  expect_that(tr, is_expected_tree(5, "1(2(4 5) 3)"))
  expect_that(sort(tr$indices), equals(0:4))

  ## Extra -- out of bounds check should fail:
  expect_that(tr$insert_at_node(10, 5), throws_error())
})
