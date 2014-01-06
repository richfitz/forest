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
## copy.
test_that("Can copy trees", {
  tr <- new(itree)
  tr1 <- tr$copy()

  ## TODO: Find out how to overload the S4 '==' method, perhaps just
  ## pointing at this method.
  expect_that(tr$is_equal_to(tr1), is_true())

  tr$insert_root(1)
  tr2 <- tr$copy()
  expect_that(tr$is_equal_to(tr1), is_false())
  expect_that(tr$is_equal_to(tr2), is_true())

  tr$insert_at_node(0, 2)
  tr$insert_at_node(0, 3)
  tr3 <- tr$copy()
  expect_that(tr$is_equal_to(tr1), is_false())
  expect_that(tr$is_equal_to(tr2), is_false())
  expect_that(tr$is_equal_to(tr3), is_true())
})

test_that("Tree subtree assignment works", {
  # NOTE: This will be easier when we get nicer constructors.
  tr <- new(itree, 5)
  tr$insert_at_node(0, 6)
  tr$insert_at_node(0, 7)
  tr$insert_at_node(2, 8)
  .tr <- tr$copy() # save for later

  ## Note that this is going to require on insert that the indices are
  ## updated in the *recieving* tree.  That should be easy enough to
  ## do...
  tr2 <- new(itree, 1)
  tr2$insert_at_node(0, 2)
  tr2$insert_at_node(0, 3)

  tr$insert_at(1L, tr$at(2L))
  expect_that(tr, is_expected_tree(5, "5(7(8) 7(8))"))

  tr$insert_at(2L, tr2)
  expect_that(tr, is_expected_tree(6, "5(7(8) 1(2 3))"))

  ## And again with the nice syntactic sugar:
  tr <- .tr

  tr[[1]] <- tr[[2]]
  expect_that(tr, is_expected_tree(5, "5(7(8) 7(8))"))

  tr[[2]] <- tr2
  expect_that(tr, is_expected_tree(6, "5(7(8) 1(2 3))"))
})

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

test_that("Can build tree using append_node", {
  tr <- new(itree, 42)
  tr$append_node(1)
  tr$append_node(1)
  expect_that(tr, is_expected_tree(3, "42(1 1)"))

  tr1 <- new(itree)
  tr1$insert_at_iterator(tr1$end(), 1)
  tr1$append_node(2)
  tr1[[1]]$append_node(3)
  tr1$append_node(4)
  expect_that(tr1, is_expected_tree(4, "1(2(3) 4)"))
})

test_that("Can build tree using prepend_node", {
  tr <- new(itree, -1)
  tr$prepend_node(42)
  expect_that(tr, is_expected_tree(2, "-1(42)"))

  front <- "-1("
  back <- "42)"
  for (i in seq_len(10)) {
    back <- paste0(i, " ", back)
    tr$prepend_node(i)
    expect_that(tr, is_expected_tree(i + 2, paste0(front, back)))
  }

  tr1 <- new(itree, -1)
  front <- "-1"
  back <- ""
  it <- tr1$begin_sub()
  for (i in 1:10) {
    front <- paste0(front, "(", i)
    back <- paste0(")", back)
    it$value$prepend_node(i)
    it$increment()
    expect_that(tr1, is_expected_tree(i + 1, paste0(front, back)))
  }
})

test_that("Can build tree using append_subtree", {
  tr <- new(itree, 42)
  tr$append_subtree(tr$copy()) # need to copy to avoid infinite regress
  expect_that(tr, is_expected_tree(2, "42(42)"))

  tr$append_subtree(new(itree, 42))
  expect_that(tr, is_expected_tree(3, "42(42 42)"))

  tr$append_subtree(tr$copy())
  expect_that(tr, is_expected_tree(6, "42(42 42 42(42 42))"))

  tr$append_subtree(tr$copy())
  str <- "42(42 42 42(42 42) 42(42 42 42(42 42)))"
  expect_that(tr, is_expected_tree(12, str))

  tr[[1]]$append_subtree(tr[[3]])
  str <- "42(42(42(42 42)) 42 42(42 42) 42(42 42 42(42 42)))"
  expect_that(tr, is_expected_tree(15, str))
})

test_that("Can build tree using prepend_subtree", {
  tr <- new(itree, 42)
  tr$prepend_subtree(tr$copy())
  expect_that(tr, is_expected_tree(2, "42(42)"))

  tr$prepend_subtree(new(itree, 42))
  expect_that(tr, is_expected_tree(3,"42(42 42)"))

  tr$prepend_subtree(tr$copy())
  expect_that(tr, is_expected_tree(6,"42(42(42 42) 42 42)"))

  tr$prepend_subtree(tr$copy())
  str <- "42(42(42(42 42) 42 42) 42(42 42) 42 42)"
  expect_that(tr, is_expected_tree(12, str))

  tr[[1]]$prepend_subtree(tr[[2]])
  str <- "42(42(42(42 42) 42(42 42) 42 42) 42(42 42) 42 42)"
  expect_that(tr, is_expected_tree(15, str))
})

test_that("In-place initialisation of integer trees possible", {
  tr1 <- tree_of(1)()
  expect_that(tr1$is_equal_to(new(itree, 1)), is_true())
  expect_that(tr1, is_expected_tree(1, "1"))

  tr2 <- tree_of(1)(tree_of(2)(3,4),5)()
  expect_that(tr2, is_expected_tree(5, "1(2(3 4) 5)"))

  tr3 <- tree_of(1)(2,tree_of(3)(tree_of(4)(5)))()
  expect_that(tr3, is_expected_tree(5, "1(2 3(4(5)))"))

  tr4 <- tree_of(1)(2,3,4)()
  expect_that(tr4, is_expected_tree(4, "1(2 3 4)"))

  tr5 <- tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6)()
  expect_that(tr5, is_expected_tree(6, "1(2 3(4(5)) 6)"))

  tr6 <- tree_of(1)(2,3,4,5,6,7,8,9,10,11)()
  expect_that(tr6, is_expected_tree(11, "1(2 3 4 5 6 7 8 9 10 11)"))
})

test_that("In-place initialisation of integer trees possible (alt)", {
  tr1 <- tree.of(1)
  expect_that(tr1$is_equal_to(new(itree, 1)), is_true())
  expect_that(tr1, is_expected_tree(1, "1"))

  tr2 <- tree.of(1, tree.of(2, 3,4),5)
  expect_that(tr2, is_expected_tree(5, "1(2(3 4) 5)"))

  tr3 <- tree.of(1, 2,tree.of(3, tree.of(4, 5)))
  expect_that(tr3, is_expected_tree(5, "1(2 3(4(5)))"))

  tr4 <- tree.of(1, 2,3,4)
  expect_that(tr4, is_expected_tree(4, "1(2 3 4)"))

  tr5 <- tree.of(1, 2,tree.of(3, tree.of(4, 5)),6)
  expect_that(tr5, is_expected_tree(6, "1(2 3(4(5)) 6)"))

  tr6 <- tree.of(1, 2,3,4,5,6,7,8,9,10,11)
  expect_that(tr6, is_expected_tree(11, "1(2 3 4 5 6 7 8 9 10 11)"))
})

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
