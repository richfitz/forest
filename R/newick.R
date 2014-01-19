##' Load and save a tree in Newick parenthetic format.
##'
##' For now, these only operate on \emph{strings}, rather than dealing
##' with files.  This, and the names of the functions, willl likely
##' change in future versions of forest.
##'
##' @title Read and Write Trees in Newick Parenthetic Format
##' @param str A string encoding a tree in Newick format.
##' @rdname newick
##' @author Rich FitzJohn
##' @export
from.newick.string <- function(str)
  from_newick_string(split.newick.string(str))
##' @rdname newick
##' @param tree A forest tree (currently class \code{xtree})
##' @param digits Number of digits to format branch lengths to.  The
##' default is the same as ape.
##' @export
to.newick.string <- function(tree, digits=10)
  to_newick_string(tree, digits)

split.newick.string <- function(str) {
  if (length(str) > 1)
    str <- paste(str, collapse="")
  str <- sub("^[[:space:]]+", "", str, perl=TRUE)
  tokens <- strsplit(str, "(?=[(),;])", perl=TRUE)[[1]]
  sub("[[:space:]]+$", "", tokens, perl=TRUE)
}
