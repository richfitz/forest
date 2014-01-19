##' @useDynLib forest
##' @import methods
##' @import Rcpp
##' @export itree
##' @export xtree
##' @export xnode

loadModule("iterator_wrapper_test", TRUE)
loadModule("tree_test",             TRUE)
loadModule("forest",                TRUE)
