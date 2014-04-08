\name{tree_brace}
\alias{tree_brace}
\title{Add Brace To Plotted Tree}
\usage{
tree_brace(label, offset = unit(0.5, "lines"), name = NULL, gp = gpar())
}
\arguments{
  \item{label}{Label in the tree to attach the brace to.
  The brace will span all descendents of the node (if
  internal) or just the single species (if terminal).
  \emph{NB} this is \emph{not} the label that will be
  eventually drawn next to the brace.}
}
\description{
Put a brace around a single clade.
}
\details{
This is going to change, but exists as a holding place so
that I can develop the lower level code.  Eventually this
is going to be subsumed within things like the taxonomic
alignment, but \emph{that} will probably work by using a
series of clades anyway, so we're unlikely to be
outrageously off.

Eventually this has to handle a brace (type [straight line,
square bracket, curly bracket], offset from tree, graphical
styling [colour, line type, line thickness]) and a label
for the brace (position [centred, top, bottom], graphical
styling [colour, font, etc], text alignment, offset from
brace).  So this function will break into pieces as
development proceeds.  For now, starting with the simplest
cases and moving forward.
}
\author{
Rich FitzJohn
}

