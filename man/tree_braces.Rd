% Generated by roxygen2 (4.0.1): do not edit by hand
\name{tree_braces}
\alias{tree_braces}
\title{Add Brace To Plotted Tree}
\usage{
tree_braces(label, offset = unit(0.5, "lines"), alignment = "none",
  name = NULL, gp = gpar())
}
\arguments{
\item{label}{Label in the tree to attach the brace to.  The
brace will span all descendents of the node (if internal) or just
the single species (if terminal).  \emph{NB} this is \emph{not}
the label that will be eventually drawn next to the brace.}

\item{offset}{A \code{unit} object describing offset in
the \emph{time} axis (positive is forward in time away from the
tip/node).}

\item{alignment}{Alignment of multiple braces, which has a visual
effect only on non-ultrametric trees.  Possible options are "none"
(the default; all braces are put 'offset' away from the longest
tip for each brace), "set" (all braces are put 'offset' away from
the longest tip among all tips spaned by braces in this set of
braces) and "global" (all braces are put 'offset' away from the
longest tip in the tree).}

\item{name}{Name for the brace (optional)}

\item{gp}{Graphical parameters (optional)}
}
\description{
Put a brace around a single clade.
}
\details{
This is going to change, but exists as a holding place so that I
can develop the lower level code.  Eventually this is going to be
subsumed within things like the taxonomic alignment, but
\emph{that} will probably work by using a series of clades anyway,
so we're unlikely to be outrageously off.

Eventually this has to handle a brace (type [straight line, square
bracket, curly bracket], offset from tree, graphical styling
[colour, line type, line thickness]) and a label for the brace
(position [centred, top, bottom], graphical styling [colour, font,
etc], text alignment, offset from brace).  So this function will
break into pieces as development proceeds.  For now, starting with
the simplest cases and moving forward.
}
\author{
Rich FitzJohn
}

