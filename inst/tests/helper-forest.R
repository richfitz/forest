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
