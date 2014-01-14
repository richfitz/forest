source("helper-forest.R")

context("Interrogation")

tree_of <- make.tree_of(xtree)
nd <- function(...) new(xnode, ...)

test_that("is_binary_tree", {
  tr0 <- new(xtree)
  expect_that(is_binary_tree(tr0), throws_error())

  tr1 <- tree_of(nd("a"))()
  expect_that(is_binary_tree(tr), throws_error())

  tr2 <- tree_of(nd("a"))(nd("b"),nd("c"))()
  expect_that(is_binary_tree(tr2), is_true())

  tr3 <- tree_of(nd("a"))(nd("b"),nd("c"),nd("d"))()
  expect_that(is_binary_tree(tr3), is_false())

  tr4 <- tree_of(nd("6"))(tree_of(nd("7"))(nd("1"), nd("2")),
                       tree_of(nd("8"))(nd("3"),
                                        tree_of(nd("9"))(nd("4"), nd("5"))))()
  expect_that(is_binary_tree(tr4), is_true())

  tr5 <- tree_of(nd("6"))(tree_of(nd("7"))(nd("1"), nd("2"), nd("10")),
                       tree_of(nd("8"))(nd("3"),
                                        tree_of(nd("9"))(nd("4"), nd("5"))))()
  expect_that(is_binary_tree(tr5), is_false())
})
