##' @useDynLib forest
##' @import methods
##' @import Rcpp
##' @export rnode
##' @export rtree

# These will become optionally compiled/loaded soon
loadModule("test_iterator_wrapper", TRUE)
loadModule("test_tree",             TRUE)

loadModule("forest",                TRUE)
loadModule("models",                TRUE)
