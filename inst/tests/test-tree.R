source("helper-forest.R")

# These tests will slowly duplicate those in
# src/treetree/test_runner.cpp

context("Basic tree operations")

test_that("Tree built with empty constructor is empty", {
  tr <- new(itree)
  expect_that(tr, is_expected_tree(0, ""))

  # Not sure what these lines are for, but equivalent ones exist in
  # tree_empty_ctor
  it <- tr$begin()
  tr1 <- tr$copy()
  it1 <- tr$begin()
})

test_that("Tree with root only is valid", {
  tr <- new(itree, 42)
  expect_that(tr, is_expected_tree(1, "42"))
  expect_that(tr$root()$data, equals(42))
})

test_that("Can construct a tree via insertion (iterators)", {
  tr <- new(itree)

  tr$insert_at_iterator(tr$end(), 1)
  expect_that(tr, is_expected_tree(1, "1"))

  tr$insert_at_iterator(tr$end_child(), 2)
  expect_that(tr, is_expected_tree(2, "1(2)"))

  tr$insert_at_iterator(tr$end_child(), 3)
  expect_that(tr, is_expected_tree(3, "1(2 3)"))

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
  expect_that(tr, is_same_tree_as(tr1))

  tr$insert_at_iterator(tr$end(), 1)
  tr2 <- tr$copy()
  expect_that(tr, is_same_tree_as(tr2))
  expect_that(tr, is_different_tree_to(tr1)) # (extra)

  tr$insert_at_iterator(tr$end_child(), 2)
  tr$insert_at_iterator(tr$end_child(), 3)
  tr3 <- tr$copy()
  expect_that(tr, is_same_tree_as(tr3))
  expect_that(tr, is_different_tree_to(tr1)) # (extra)
  expect_that(tr, is_different_tree_to(tr2)) # (extra)
})

# tree_assignment -- not done because not how R works

test_that("Tree subtree assignment works", {
  tr <- tree_of(5)(6,tree_of(7)(8))()
  tr2 <- tree_of(1)(2,3)()

  tr[[1]] <- tr[[2]]
  expect_that(tr, is_expected_tree(5, "5(7(8) 7(8))"))

  tr[[2]] <- tr2
  expect_that(tr, is_expected_tree(6, "5(7(8) 1(2 3))"))
})

test_that("Arity calculations are correct", {
  tr <- tree_of(1)(2,3,tree_of(4)(5))()
  expect_that(tr, is_expected_tree(5, "1(2 3 4(5))"))

  expect_that(tr$arity, equals(3))
  expect_that(tr$front_sub()$arity, equals(0))
  expect_that(tr[[2]]$arity, equals(0))
  expect_that(tr[[3]]$arity, equals(1))
  expect_that(tr[[3]][[1]]$arity, equals(0))
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
  expect_that(tr1, is_same_tree_as(new(itree, 1)))
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

# This is extra, but uses an alternative tree initialiser:
test_that("In-place initialisation of integer trees possible (alt)", {
  tr1 <- tree.of(1)
  expect_that(tr1, is_same_tree_as(new(itree, 1)))
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

test_that("Can compare trees for equality", {
  tr <- list(
    tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6)(),
    tree_of(1)(2,tree_of(3)(tree_of(0)(5)),6)(),
    tree_of(1)(2,tree_of(3)(tree_of(4)(5)),0)(),
    tree_of(1)(2,tree_of(3)(tree_of(4)(0)),6)(),
    tree_of(1)(2,tree_of(3)(tree_of(4)(5,7)),6)(),
    tree_of(1)(2,tree_of(3)(tree_of(4)),5,6)(),
    tree_of(1)(2,tree_of(3)(tree_of(4)(5)),6,7)(),
    tree_of(1)(2,3,tree_of(4)(5),6)(),
    new(itree),
    tree_of(1)())

  tr[[1]]$append_node(42)
  it <- tr[[1]]$end_child()
  it$decrement()
  tr[[1]]$erase(it)

  for (i in seq_along(tr)) {
    for (j in seq_along(tr)) {
      if (i==j)
        expect_that(tr[[i]], is_same_tree_as(tr[[i]]$copy()))
      else
        expect_that(tr[[i]], is_different_tree_to(tr[[j]]))
    }
  }

  ## TODO: implement the "equal" function, taking a comparison
  ## predicate.  That tests structure, not contents.  Pretty cool.

  a <- tree_of(1)(tree_of(2)(3,4,5))()
  b <- tree_of(1)(tree_of(2)(tree_of(3)(4,5)))()
  expect_that(a[[1]][[1]], is_different_tree_to(b[[1]][[1]]))
})

## tree_less; already decided not to implement (as not relevant to
##   phylogenetic work)

test_that("Tree constant iterators", {
  tr <- tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8)()

  pre.contents <- seq(1, 8)
  pre.subtr <-
    list(tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8)(),
         tree_of(2)(),
         tree_of(3)(tree_of(4)(5),6)(),
         tree_of(4)(5)(),
         tree_of(5)(),
         tree_of(6)(),
         tree_of(7)(),
         tree_of(8)())
  child.contents <- list(2, 3, 7, 8)
  child.subtr <-
    list(tree_of(2)(),
         tree_of(3)(tree_of(4)(5),6)(),
         tree_of(7)(),
         tree_of(8)())
  post.contents <- list(2, 5, 4, 6, 3, 7, 8, 1)
  post.subtr <- list(
      tree_of(2)(),
      tree_of(5)(),
      tree_of(4)(5)(),
      tree_of(6)(),
      tree_of(3)(tree_of(4)(5),6)(),
      tree_of(7)(),
      tree_of(8)(),
      tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8)())

  ## This is uglier than needed because iterators are not generic yet,
  ## and we don't have iterators over vectors (could get that working
  ## later with package iterators).  Plus the wrapped node causes
  ## problems here, still.
  check_range <- function(f1, l1, cmp) {
    if (distance(f1, l1) != length(cmp))
      return(FALSE)
    i <- 1
    while (f1$differs(l1)) {
      if (inherits(f1$value, "Rcpp_inode")) {
        if (f1$value$data != cmp[[i]])
          return(FALSE)
      } else {
        if (!f1$value$is_equal_to(cmp[[i]]))
          return(FALSE)
      }
      f1$increment()
      i <- i + 1
    }
    TRUE
  }

  distance <- forest:::distance_itree_pre_iterator
  expect_that(check_range(tr$begin(), tr$end(), pre.contents),
              is_true())

  distance <- forest:::distance_itree_sub_pre_iterator
  expect_that(check_range(tr$begin_sub(), tr$end_sub(), pre.subtr),
              is_true())

  distance <- forest:::distance_itree_child_iterator
  expect_that(check_range(tr$begin_child(), tr$end_child(),
                          child.contents),
              is_true())

  distance <- forest:::distance_itree_sub_child_iterator
  expect_that(check_range(tr$begin_sub_child(), tr$end_sub_child(),
                          child.subtr),
              is_true())

  distance <- forest:::distance_itree_post_iterator
  expect_that(check_range(tr$begin_post(), tr$end_post(),
                          post.contents),
              is_true())

  distance <- forest:::distance_itree_sub_post_iterator
  expect_that(check_range(tr$begin_sub_post(), tr$end_sub_post(),
                          post.subtr),
              is_true())
})

## tree_const_iters_backwards; not really needed -- this is just a
##   boost reversable iterator wrapper around iterators.  Do this if
##   we get nice iterators support, perhaps?

## tree_mutable_iters; needs considerably more support.

test_that("Tree accessors work", {
  tr <- tree_of(1)(2,
                   tree_of(3)(
                     tree_of(
                       4)(
                         5),
                     6),
                   7,
                   8)()

  ## TODO: None of these are written yet:
  expect_that(tr$root()$data,  equals(1))
  expect_that(tr$front()$data, equals(2))
  expect_that(tr$back()$data,  equals(8))
  expect_that(tr$front_sub(), is_same_tree_as(tree_of(2)()))
  expect_that(tr$back_sub(),  is_same_tree_as(tree_of(8)()))

  cmp <- tree_of(1)(2,tree_of(3)(tree_of(4)(5),6),7,8)()
  expect_that(tr$root_sub(), is_same_tree_as(cmp))

  expect_that(tr[[1]],           is_same_tree_as(tree_of(2)()))
  expect_that(tr[[2]],           is_same_tree_as(tree_of(3)(tree_of(4)(5),6)()))
  expect_that(tr[[2]][[1]],      is_same_tree_as(tree_of(4)(5)()))
  expect_that(tr[[2]][[1]][[1]], is_same_tree_as(tree_of(5)()))
  expect_that(tr[[2]][[2]],      is_same_tree_as(tree_of(6)()))
  expect_that(tr[[3]],           is_same_tree_as(tree_of(7)()))
  expect_that(tr[[4]],           is_same_tree_as(tree_of(8)()))
})

## tree_insert2; not checked what is different here to tree_insert
##   yet.

## tree_append3, tree_prepend3: Iterator pair versions of append,
##   prepend -- should not be too hard.

## tree_append4, tree_prepend4: multiply append trees -- not sure if
##   that makes any sense for us.

## tree_append5, tree_prepend5: append vectors of trees -- could be
##   useful.

test_that("tree_insert_above", {
  tr <- new(itree, 42)
  tr$insert_above(tr$begin(), 41)
  expect_that(tr, is_expected_tree(2, "41(42)"))

  tr$insert_above(tr[[1]]$begin(), 40)
  expect_that(tr, is_expected_tree(3, "41(40(42))"))

  tr[[1]]$append_node(43)
  tr$insert_above(tr[[1]]$begin_child(), 39)
  expect_that(tr, is_expected_tree(5, "41(40(39(42) 43))"))
})

test_that("tree_insert_below", {
  tr <- new(itree, 42)

  tr$insert_below(tr$begin(),41)
  expect_that(tr, is_expected_tree(2, "42(41)"))

  tr$insert_below(tr$begin(),40)
  expect_that(tr, is_expected_tree(3, "42(40(41))"))

  tr[[1]]$append_node(43)
  tr$insert_below(tr[[1]]$begin(), 39)
  expect_that(tr, is_expected_tree(5, "42(40(39(41 43)))"))
})

test_that("tree_insert_flatten", {
  tr <- tree_of(0)(tree_of(1)(2,tree_of(3)(tree_of(4)(5,6))))()

  tr$flatten(tr$begin_child())
  expect_that(tr,is_expected_tree(7, "0(1 2 3(4(5 6)))"))

  tmp <- tr$copy()

  tr$flatten(tr[[3]]$begin())
  expect_that(tr, is_expected_tree(7, "0(1 2 3 4(5 6))"))

  tmp$flatten(tmp[[3]]$begin_child())
  expect_that(tmp, is_expected_tree(7, "0(1 2 3(4 5 6))"))

  tmp$flatten(tmp[[3]]$begin())
  expect_that(tmp, is_expected_tree(7, "0(1 2 3 4 5 6)"))
})

test_that("tree_erase1", {
  tr <- tree_of(1)(tree_of(2)(3,4,42),
                   tree_of(5)(tree_of(6)(7,8)),
                   tree_of(9)(tree_of(10)(11),12))()
  str <- "1(2(3 4 42) 5(6(7 8)) 9(10(11) 12))"
  expect_that(tr, is_expected_tree(13, str))

  tr$erase(tr[[2]][[1]])
  expect_that(tr, is_expected_tree(10, "1(2(3 4 42) 5 9(10(11) 12))"))

  tr$erase(tr[[3]][[2]])
  expect_that(tr, is_expected_tree(9, "1(2(3 4 42) 5 9(10(11)))"))

  tr$erase(tr[[3]][[1]][[1]])
  expect_that(tr, is_expected_tree(8, "1(2(3 4 42) 5 9(10))"))

  tr$erase(tr[[1]][[2]])
  expect_that(tr, is_expected_tree(7, "1(2(3 42) 5 9(10))"))

  tr$erase(tr[[1]][[1]])
  expect_that(tr, is_expected_tree(6, "1(2(42) 5 9(10))"))

  tr$erase(tr$begin())
  expect_that(tr, is_expected_tree(0, ""))
})

test_that("tree_erase2", {
  tr <- tree_of(1)(tree_of(5)(tree_of(6)(7,8)),
                   tree_of(9)(tree_of(10)(11),12),
                   tree_of(2)(3,4,42))()
  str <- "1(5(6(7 8)) 9(10(11) 12) 2(3 4 42))"
  expect_that(tr, is_expected_tree(13, str))

  tr$erase_pair(tr[[1]]$begin_child(), tr[[1]]$end_child())
  expect_that(tr, is_expected_tree(10,"1(5 9(10(11) 12) 2(3 4 42))"))

  tr$erase_pair(tr[[1]]$begin_child(), tr[[1]]$end_child())
  expect_that(tr, is_expected_tree(10,"1(5 9(10(11) 12) 2(3 4 42))"))

  it <- tr$begin_child()
  it$increment()
  tr$erase_pair(it, tr$end_child())
  expect_that(tr, is_expected_tree(2, "1(5)"))

  tr$erase_pair(tr$begin_child(), tr$end_child())
  expect_that(tr, is_expected_tree(1, "1"))
})

test_that("tree_prune", {
  tr <- tree_of(1)(tree_of(2)(3,4),
                   tree_of(5)(tree_of(6)(7,8)),
                   tree_of(9)(tree_of(10)(11),12))()
  str <- "1(2(3 4) 5(6(7 8)) 9(10(11) 12))"
  expect_that(tr, is_expected_tree(12, str))

  tr[[1]]$prune()
  expect_that(tr, is_expected_tree(10,"1(2 5(6(7 8)) 9(10(11) 12))"))

  tr[[3]][[1]]$prune()
  expect_that(tr, is_expected_tree(9, "1(2 5(6(7 8)) 9(10 12))"))

  tr$prune()
  expect_that(tr, is_expected_tree(1, "1"))
})

test_that("tree_clear", {
  tr <- new(itree, 42)
  ci <- tr$begin_child()

  tr$clear()
  expect_that(tr, is_expected_tree(0, ""))

  tr$clear()
  expect_that(tr, is_expected_tree(0, ""))

  tr$insert_at_iterator(tr$end(), 1)
  tr$clear()
  expect_that(tr, is_expected_tree(0, ""))

  tr$insert_at_iterator(tr$end(),1)
  # TODO: Needs these append methods still:
  # tr$append(20,tree_of(1)(2,3,tree_of(4)(5))())
  # tr[[1]]$append(5, tr$copy())
  tr$clear()
  expect_that(tr, is_expected_tree(0, ""))
})

test_that("tree_splice1", {
  tr1 <- tree_of(1)(2,3)()
  tr2 <- tree_of(4)()
  tr3 <- tree_of(5)(6,tree_of(7)(8))()

  tr1$splice(tr1[[1]]$begin(),tr2)
  expect_that(tr2, is_expected_tree(0, ""))
  expect_that(tr1, is_expected_tree(4, "1(4 2 3)"))

  tr1$splice(tr1$end_child(), tr1[[1]])
  expect_that(tr1, is_expected_tree(4, "1(2 3 4)"))

  tr1$splice(tr1[[2]]$begin(), tr1$back_sub())
  expect_that(tr1, is_expected_tree(4, "1(2 4 3)"))

  tr1$splice(tr1$end_child(), tr1[[2]])
  expect_that(tr1, is_expected_tree(4, "1(2 3 4)"))

  it <- tr1$begin_child()
  it$increment()
  tr1$splice(it, tr3[[2]])
  expect_that(tr1, is_expected_tree(6, "1(2 7(8) 3 4)"))
  expect_that(tr3, is_expected_tree(2, "5(6)"))

  tr1$splice(tr3$end_child(), tr1[[2]])
  expect_that(tr1, is_expected_tree(4, "1(2 3 4)"))
  expect_that(tr3, is_expected_tree(4, "5(6 7(8))"))

  it <- tr1$end()
  it$decrement()
  tr1$splice(it, tr3)
  expect_that(tr1, is_expected_tree(8, "1(2 3 5(6 7(8)) 4)"))
  expect_that(tr3, is_expected_tree(0, ""))

  tr4 <- tree_of(1)(2)()
  tr1$splice(tr1$end_child(), tr4$begin_child())
  expect_that(tr4, is_expected_tree(1, "1"))
  expect_that(tr1, is_expected_tree(9, "1(2 3 5(6 7(8)) 4 2)"))

  tr4$splice(tr4$end_child(), tr1)
  expect_that(tr1, is_expected_tree(0, ""))
  expect_that(tr4, is_expected_tree(10, "1(1(2 3 5(6 7(8)) 4 2))"))
})

test_that("tree_splice2",  {
  tr1 <- tree_of(1)(2,3)()
  tr2 <- tree_of(4)()
  tr3 <- tree_of(5)(6,tree_of(7)(8))()

  tr1$splice_pair(tr1[[1]]$begin(),
                  tr2$begin_child(), tr2$end_child())
  expect_that(tr1, is_expected_tree(3, "1(2 3)"))
  expect_that(tr2, is_expected_tree(1, "4"))

  tr1$splice_pair(tr1$end_child(),
                  tr3$begin_child(), tr3$end_child())
  expect_that(tr1, is_expected_tree(6, "1(2 3 6 7(8))"))
  expect_that(tr3, is_expected_tree(1, "5"))

  it1 <- tr1$begin_child()
  it1$increment()
  it2 <- tr1$end_child()
  it2$decrement()
  tr3$splice_pair(tr3$end_child(), it1, it2)
  expect_that(tr1, is_expected_tree(4, "1(2 7(8))"))
  expect_that(tr3, is_expected_tree(3, "5(3 6)"))
})

## tree_swap

## tree_parent

## tree_io, tree_io_empty, tree_out_sexpr, tree_in_sexpr

## tree_node_destructor
