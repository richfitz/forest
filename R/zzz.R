##' @useDynLib forest
##' @import methods
##' @import Rcpp
##' @export itree
##' @export xtree
##' @export xnode
##' @export rnode

loadModule("iterator_wrapper_test", TRUE)
loadModule("tree_test",             TRUE)
loadModule("forest",                TRUE)
loadModule("models",                TRUE)
loadModule("simple",                TRUE)
