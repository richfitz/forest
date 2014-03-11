\name{style_thing}
\alias{style_branches}
\alias{style_node_labels}
\alias{style_thing}
\alias{style_tip_labels}
\title{Style Tree By Node}
\usage{
style_thing(tree_grob, what, ..., base = NULL)

style_branches(tree_grob, ...)

style_tip_labels(tree_grob, ...)

style_node_labels(tree_grob, ...)
}
\arguments{
  \item{tree_grob}{A tree grob, created by
  \code{\link{treeGrob}}}

  \item{what}{The child member of \code{tree_grob} to
  style.  At present, values of "branches", "tip_labels"
  and "node_labels" are supported, but any child member of
  \code{tree_grob} could be used here.  A vector of names
  is allowed.}

  \item{...}{Named graphical parameters.  E.g., pass in
  \code{node5=gpar(col="red")} will colour all descendents
  of "node5" red.}

  \item{base}{}
}
\value{
A tree grob
}
\description{
Apply graphics parameters to parts of a tree, varying based
on parent nodes.  The tree gets split into different
regimes; there is a base regime starting at the root, and
then a series of regimes painted onto the tree using the
MEDUSA algorithm.  These regimes can be nested.
}
\details{
Generally, use the wrapper functions
(\code{style_branches}, \code{style_tip_labels},
\code{style_node_labels}) but the low-level
\code{style_thing} allows changing graphical parameters of
any member of the tree grob with a \code{labels} member.
}
\author{
Rich FitzJohn
}

