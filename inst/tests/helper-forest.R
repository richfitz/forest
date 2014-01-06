library(methods)
library(Rcpp)
library(testthat)
library(forest)

is_expected_tree <- function(n, representation) {
  function(tr) {
    ok <- (isTRUE(all.equal(tr$size, n))           &&
           isTRUE(all.equal(tr$empty, n == 0))     &&
           isTRUE(all.equal(tr$childless, n <= 1)) &&
           isTRUE(all.equal(tr$representation, representation)))
    expectation(ok, "Tree does not have expected contents")
  }
}

is_same_tree_as <- function(cmp) {
  function(tr)
    expectation(tr$is_equal_to(cmp), "Trees differ")
}

is_different_tree_to <- function(cmp) {
  function(tr)
    expectation(!tr$is_equal_to(cmp), "Trees are the same")
}


## This version is more like the C++ version, but needs to be
## terminated by () to return the tree.
tree_of <- function(v, ...) {
  tr <- new(itree, v)

  ## TODO: Write a method of a tree that determines if something is
  ## node like -- we could do this by trying to insert it as a tree
  ## wrapped with try/catch, perhaps...
  x <- function(c) {
    if (is.integer(c) || is.numeric(c))
      tr$prepend_node(c)
    else if (inherits(c, "tree.generator"))
      tr$prepend_subtree(c())
    else
      tr$prepend_subtree(c)
  }

  rec <- function(...) {
    tail <- list(...)
    if (length(tail) == 0)
      return(tr)
    for (i in rev(tail))
      x(i)
    rec
  }

  class(rec) <- "tree.generator"
  rec
}

## This version does not require the terminating parenthesis, but
## lends itself to more awkward syntax.
tree.of <- function(head, ...) {
  tr <- new(itree, head)

  x <- function(c) {
    if (is.integer(c) || is.numeric(c))
      tr$prepend_node(c)
    else
      tr$prepend_subtree(c)
  }

  for (i in rev(list(...)))
    x(i)

  tr
}
