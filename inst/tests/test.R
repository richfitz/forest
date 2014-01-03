source("helper-forest.R")

# These tests will slowly duplicate those in
# src/treetree/test_runner.cpp

test_that("Tree built with empty constructor is empty", {
  tr <- new(itree)
  expect_that(tr$empty, is_true())
  expect_that(tr$size, equals(0))
  expect_that(tr$childless, is_true())
  expect_that(tr$representation, is_identical_to(""))
})

test_that("Tree with root only is valid", {
  tr <- new(itree, 42)
  expect_that(tr$empty, is_false())
  expect_that(tr$size, equals(1))
  expect_that(tr$childless, is_true())
  expect_that(tr$representation, is_identical_to("42"))
})

## These are going to have to get improved later; there's not
## currently a good way of accessing the nodes to get the proper
## insertion operators working.
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

  tr$insert_end(1)
  expect_that(tr, is_expected_tree(1, "1"))
  
  tr$insert_end_child(2)
  expect_that(tr, is_expected_tree(2, "1(2)"))

  tr$insert_end_child(3)
  expect_that(tr, is_expected_tree(3, "1(2 3)"))
})
