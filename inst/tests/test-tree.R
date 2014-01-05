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

test_that("Can construct a tree via insertion (indices)", {
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

test_that("Can construct a tree via insertion (iterators)", {
  tr <- new(itree)

  tr$insert_at_iterator(tr$end(), 1)
  expect_that(tr, is_expected_tree(1, "1"))
  expect_that(tr$arity, equals(0))

  tr$insert_at_iterator(tr$end_child(), 2)
  expect_that(tr, is_expected_tree(2, "1(2)"))
  expect_that(tr$arity, equals(1))

  tr$insert_at_iterator(tr$end_child(), 3)
  expect_that(tr, is_expected_tree(3, "1(2 3)"))
  expect_that(tr$arity, equals(2))

  tr$insert_at_iterator(tr$begin_sub_child()$value$end_child(), 4)
  expect_that(tr, is_expected_tree(4, "1(2(4) 3)"))

  tr$insert_at_iterator(tr$end_child(), 5)
  expect_that(tr, is_expected_tree(5, "1(2(4) 3 5)"))

  tr$insert_at_iterator(tr$begin_sub_child()$value$begin_child(), 6)
  expect_that(tr, is_expected_tree(6, "1(2(6 4) 3 5)"))
})

## This is not actually the same test as tree_copy_ctor or
## tree_assignment in test_runner.cpp, because we're not going to test
## the copy constructor there.  Instead here we test the ability to
## clone.
test_that("Can copy trees", {
  tr <- new(itree)
  tr1 <- tr$clone()

  ## TODO: Find out how to overload the S4 '==' method, perhaps just
  ## pointing at this method.
  expect_that(tr$is_equal_to(tr1), is_true())

  tr$insert_root(1)
  tr2 <- tr$clone()
  expect_that(tr$is_equal_to(tr1), is_false())
  expect_that(tr$is_equal_to(tr2), is_true())

  tr$insert_at_node(0, 2)
  tr$insert_at_node(0, 3)
  tr3 <- tr$clone()
  expect_that(tr$is_equal_to(tr1), is_false())
  expect_that(tr$is_equal_to(tr2), is_false())
  expect_that(tr$is_equal_to(tr3), is_true())
})

## tree_subtr_assignment looks like fun, but need to get a bit better
## with subtrees to get this to work...

test_that("Arity calculations are correct", {
  tr <- new(itree, 1)
  tr$insert_at_node(0, 2)
  tr$insert_at_node(0, 3)
  tr$insert_at_node(0, 4)
  tr$insert_at_node(3, 5)

  expect_that(tr, is_expected_tree(5, "1(2 3 4(5))"))

  expect_that(tr$arity, equals(3))
  ## then for tr$front_sub (tr[0]), tr[1], tr[2], tr[2][0], once we
  ## can get subtrees out.
})

## tree_append1
## tree_prepend1

## tree_append2
## tree_prepend2

## tree_inplace_init; not directly applicable, but we will need
##   something similar.

## tree_equaility; probably more focussed on general trees than
##   something that we will need here.

## tree_less; already decided not to implement (as not relevant to
## phylogenetic work)

## tree_const_iters; get this working in the style of itertools,
##   perhaps?  Depends what we need to be able to reach from each
##   iteration.  However, because we can do things like it->index() to
##   get the index out, this could be a base for (somewhat
##   inefficient) persistent write-able iterators in R.

## tree_const_iters_backwards (same as above)

## tree_mutable_iters; these are trickier.

## tree_accessors; this is possibly the next in line, as we'll get the
## subtree issues sorted out.
