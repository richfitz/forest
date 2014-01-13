source("helper-forest.R")

# Extra low-level things in the wrapper

context("Basic tree operations: extra")

tree_of <- make.tree_of(itree)

test_that("Can count the number of tips in a tree", {
  tr0 <- new(xtree)
  tr1 <- tree_of(5)()
  tr2 <- tree_of(5)(1)()
  tr3 <- tree_of(5)(1,2)()
  tr4 <- tree_of(5)(1,2,3)()

  expect_that(tr0$tips, equals(0))
  expect_that(tr1$tips, equals(1))
  expect_that(tr2$tips, equals(1))
  expect_that(tr3$tips, equals(2))
  expect_that(tr4$tips, equals(3))

  expect_that(tr0$nodes, equals(0))
  expect_that(tr1$nodes, equals(0))
  expect_that(tr2$nodes, equals(1))
  expect_that(tr3$nodes, equals(1))
  expect_that(tr4$nodes, equals(1))
})
