library(methods)
library(Rcpp)
library(testthat)
library(forest)
library(ape)

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

make.tree_of <- function(class) {
  prepend <- function(tr, x) {
    if (tr$is_node_type(x))
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
