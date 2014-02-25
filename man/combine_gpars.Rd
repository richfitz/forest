\name{combine_gpars}
\alias{combine_gpars}
\title{Combine Graphical Parameters}
\usage{
combine_gpars(list, index)
}
\arguments{
  \item{list}{An R list (not a \codeg{gpar} list), each
  element of which is a \code{gpar} list).  The first
  element is taken as the "base" parameters.}

  \item{index}{}
}
\description{
Combine a list of graphical parameters.
}
\details{
The idea is this; sometimes it makes sense to think of
different regions of a tree, or different lines, words, etc
having different graphical parameters, and we'd like to set
them using the \code{gpar()} function.  Something like
`list(base=gpar(col="black"),
Asteraceae=gpar(col="yellow"))` and then have these
expanded out to a single \code{gpar} object with the
\code{col} element appropriately vectorised, so that the
branches, tip labels, node labels, etc for Asteraceae will
end up yellow while everything else remains black.

This should work for any number of parameters, so that if
\code{lwd} is present in one list and not in the other,
when everything gets vectorised out, \code{lwd} is going to
be needed in both.  That means that we need to look up what
the appropriate \emph{default} value of \code{lwd} is going
to be.  We do that by using \code{get.gpar()} (if this is
used during a \code{drawDetails} method then this will
behave most predictably, otherwise it will probably end up
with the grid defaults).

Note that if no graphics window is open, this will open
one.

The first element of the list is considered to be the
\emph{base} parameters.

This function does not do validation of parameters, or that
we have even got \code{gpar} objects at this point.  Also
needing validation: each element is a scalar.
}
\author{
Rich FitzJohn
}

