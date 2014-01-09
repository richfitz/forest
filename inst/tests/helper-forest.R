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
    expectation(tr$equals(cmp), "Trees differ")
}

is_different_tree_to <- function(cmp) {
  function(tr)
    expectation(!tr$equals(cmp), "Trees are the same")
}

## TODO: Write a method of a tree that determines if something is the
## node data type -- we could do this by writing a function that
## attempts to do something with an object as if it was a node and
## throws if the conversion is not possible.  See below for what I'm
## using now though.
make.tree_of <- function(class, is.node) {
  prepend <- function(tr, x) {
    if (is.node(x))
      tr$prepend(x)
    else
      tr$prepend_subtree(if (inherits(x, "tree.generator")) x() else x)
  }

  function(v, ...) {
    tr <- new(class, v)

    rec <- function(...) {
      tail <- list(...)
      if (length(tail) == 0)
        return(tr)
      for (i in rev(tail))
        prepend(tr, i)
      rec
    }

    class(rec) <- "tree.generator"
    rec
  }
}

is.itree.node <- function(x)
  is.integer(x) || is.numeric(x)
