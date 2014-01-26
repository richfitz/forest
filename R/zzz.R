##' @useDynLib forest
##' @import methods
##' @import Rcpp
##' @export itree
##' @export rnode
##' @export rtree

loadModule("iterator_wrapper_test", TRUE)
loadModule("tree_test",             TRUE)
loadModule("forest",                TRUE)
loadModule("models",                TRUE)
