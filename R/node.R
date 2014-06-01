##' Build a node.  This is a low-level function that should not be
##' needed that often.
##'
##' Nodes are very lightly wrapped lists with the S3 class
##' \code{forest_node}.  I don't think that we need or want reference
##' semantics here and this keeps things nice and lightweight.
##' Methods have been defined for the subset and assignment operators
##' so that the object only has the elements \code{label},
##' \code{value} and \code{data}.  This will impose a minor lookup
##' cost for the assignment operators, but reading should not be
##' affected except for \code{[} (so prefer \code{\$} and \code{[[}).
##'
##' @title Build a Forest Node
##' @param label Label for the node (single-element character vector)
##' @param length Branch length from root to tip (single-element
##' numeric, non-negative)
##' @param data Arbitrary data to associate with the node.
##' @return An object of type \code{forest_node}
##' @export
##' @author Rich FitzJohn
forest_node <- function(label="", length=NA_real_, data=NULL) {
  assert_scalar(label)
  assert_character(label)
  assert_scalar(length)
  assert_number(length)
  if (!is.na(length)) {
    assert_nonnegative(length)
  }
  forest_node__ctor(label, length, data)
}

##' @export
`[[<-.forest_node` <- function(x, name, value) {
  assert_scalar(name)
  if (name == "label") {
    assert_scalar(value)
    assert_character(value)
  } else if (name == "length") {
    assert_scalar(value)
    assert_number(value)
    if (!is.na(value)) {
      assert_nonnegative(value)
    }
  } else if (name != "data") {
    valid <- paste(dQuote(c("label", "length", "data")), collapse=", ")
    stop(sprintf("Invalid name %s (must be one of %s)", dQuote(name), valid),
         call.=FALSE)
  }
  NextMethod()
}

##' @export
`$<-.forest_node` <- function(x, name, value) {
  x[[name]] <- value
  x
}

## Single-bracket indexing is potentially hard to deal with, so I'm
## just punting on it for now and passing along to the double-bracket
## indexing.  Perhaps not ideal - instead we could just throw an
## error?
##' @export
`[.forest_node` <- function(x, name) {
  x[[name]]
}

##' @export
`[<-.forest_node` <- function(x, name, value) {
  x[[name]] <- value
  x
}
