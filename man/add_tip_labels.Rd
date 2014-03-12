\name{add_tip_labels}
\alias{add_node_labels}
\alias{add_tip_labels}
\title{Add Tip and Node Labels to a Plotted Tree}
\usage{
add_tip_labels(tree_grob, name = "tip_labels", ...)

add_node_labels(tree_grob, name = "node_labels", ...)
}
\arguments{
  \item{tree_grob}{A tree grob, created by
  \code{\link{treeGrob}}}

  \item{name}{Name of the tip labels grob}

  \item{...}{Additional arguments passed through to
  underlying functions (NB: interface may change!).  Useful
  things include \code{offset}, which is a \code{unit}
  object describing offset in the \emph{time} axis
  (positive is forward in time away from the tip/node),
  \code{gp}, which is a graphical parameters list (created
  by \code{gpar}; as with \code{\link{treeGrob}}, avoid
  passing in vectors here, \code{rot}, which is the
  rotation (relative to the time axis).}
}
\description{
Add tip and node labels to a plotted tree.  The idea is to
take a tree that is created by \code{\link{treeGrob}} and
add tip labels into it.
}
\details{
The interface will change here, and this function
\emph{will} change and probably disappear.  Eventually
we're working towards a \code{ggplot}-style \code{+}
operators on tree graphical elements.
}
\author{
Rich FitzJohn
}

