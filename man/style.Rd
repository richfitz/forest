\name{tree_style}
\alias{tree_style}
\alias{tree_style_branches}
\alias{tree_style_node_labels}
\alias{tree_style_tip_labels}
\title{Style Tree By Node}
\usage{
tree_style(what, ..., base = NULL)

tree_style_branches(..., base = NULL)

tree_style_tip_labels(..., base = NULL)

tree_style_node_labels(..., base = NULL)
}
\arguments{
  \item{what}{The child member of \code{tree_grob} to
  style.  At present, values of "branches", "tip_labels"
  and "node_labels" are supported, but any child member of
  \code{tree_grob} could be used here.  A vector of names
  is allowed.}

  \item{...}{Named graphical parameters.  E.g., pass in
  \code{node5=gpar(col="red")} will colour all descendents
  of "node5" red.}

  \item{base}{Base graphical parameters (by default the
  style is the \code{gpar()}, but this will change).}
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
(\code{tree_style_branches}, \code{tree_style_tip_labels},
\code{tree_style_node_labels}) but the low-level
\code{style_thing} allows changing graphical parameters of
any member of the tree grob with a \code{labels} member.
}

