\name{tree_style}
\alias{tree_style}
\alias{tree_style_brace}
\alias{tree_style_branches}
\alias{tree_style_node_labels}
\alias{tree_style_tip_labels}
\title{Style Tree By Node}
\usage{
tree_style(class, ..., descendants = TRUE, base = NULL, name = NULL)

tree_style_branches(..., base = NULL)

tree_style_tip_labels(..., base = NULL)

tree_style_node_labels(..., base = NULL)

tree_style_brace(..., base = NULL, name = NULL)
}
\arguments{
  \item{class}{The class of a child member of
  \code{tree_grob} to style. This is passed through to
  \code{\link{tree_match}} as \code{class}, so anything
  that would be appropriate there is good here (e.g.,
  \code{tree_branches}, \code{tree_labels},
  \code{tree_braces}.}

  \item{...}{Named graphical parameters.  E.g., pass in
  \code{node5=gpar(col="red")} will colour all descendents
  of "node5" red.}

  \item{descendants}{Apply styling to all descendants?  The
  default is TRUE, using the MEDUSA algorithm to do this.
  But specifying \code{descendants=FALSE} allows styling of
  a single branch, node label, etc.  For tips this has no
  effect as they have no descendants!}

  \item{base}{Base graphical parameters (by default the
  style is the \code{gpar()}, but this will change).}

  \item{name}{Passed through to \code{tree_match} - useful
  to distinguish between multiple child members with the
  same class (such as the tip and node labels).}
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
\code{tree_style} allows changing graphical parameters of
any member of the tree grob with a \code{labels} member.
}

