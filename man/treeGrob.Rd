\name{treeGrob}
\alias{treeGrob}
\title{Create a Tree Grob}
\usage{
treeGrob(tree, direction = "right", name = NULL, gp = NULL, vp = NULL)
}
\arguments{
  \item{tree}{A forest tree.  The underlying data do not
  matter at this point (but that may change in future).
  Note that this cannot be an ape \code{phylo} tree. accept
  an ape tree, but that is}

  \item{direction}{Direction in which to plot the tree.
  Valid options are \dQuote{right} (the default),
  \dQuote{left}, \dQuote{up}, \dQuote{down} and
  \code{circle}.  The circle version plots a tree very
  similar to ape's \dQuote{fan} style.}

  \item{name}{Name of the grob (optional)}

  \item{gp}{Graphical parameters that the segments will
  take.  This one is \emph{really} up for grabs.  I'd
  suggest being fairly tame here and treating this as
  scalar values only.  There is pretty much no way that
  non-scalar values will do the right thing.  I'll sort out
  a nicer way of actually changing the colour of edges and
  things like that without relying on blindly passing in
  vectors and hoping for the best.}

  \item{vp}{A grid viewport object that will be pushed
  before the object is drawn (optional)}
}
\value{
A tree grob.
}
\description{
Plot the backbone of a forest tree.  This takes a tree and
turns it into a graphical object that can be plotted with
\code{grid.draw} (soon that part will be automated and a
wrapper will be added).
}
\section{Warning}{
  Everything up for change in the interface here. Do not
  depend on this or I will probably break your code.
}
\author{
Rich FitzJohn
}

