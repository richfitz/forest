## NOTE: Coding standards here follow `package:grid` as much as
## possible; treeGrob, in camelCase, is a "tree graphical object" etc.

## WARNING: Probably more than the rest of the package, the interface
## here will change massively as I work out what actually works
## nicely.

## TODO: Examples!  But to do that we need a tree simulator (see issue
## #30).

##'  Plot the backbone of a forest tree.  This takes a tree and turns
##' it into a graphical object that can be plotted with
##' \code{grid.draw} (soon that part will be automated and a wrapper
##' will be added).
##'
##'
##' @title Create a Tree Grob
##' @param tree A forest tree.  The underlying data do not matter at
##' this point (but that may change in future).  Note that this cannot
##' be an ape \code{phylo} tree.
##' accept an ape tree, but that is
##' @param direction Direction in which to plot the tree.  Valid
##' options are \dQuote{right} (the default), \dQuote{left},
##' \dQuote{up}, \dQuote{down} and \code{circle}.  The circle version
##' plots a tree very similar to ape's \dQuote{fan} style.
##' @param theta0 Starting point when drawing trees of direction
##' "circle" only.  May eventually be supported for "semicircle" too.
##' Silently ignored for all other directions.
##' @param name Name of the grob (optional)
##' @param gp Graphical parameters that the segments will take.  This
##' one is \emph{really} up for grabs.  I'd suggest being fairly tame
##' here and treating this as scalar values only.  There is pretty
##' much no way that non-scalar values will do the right thing.  I'll
##' sort out a nicer way of actually changing the colour of edges and
##' things like that without relying on blindly passing in vectors and
##' hoping for the best.
##' @param vp A grid viewport object that will be pushed before the
##' object is drawn (optional)
##' @return A tree grob.
##' @section Warning: Everything up for change in the interface here.
##' Do not depend on this or I will probably break your code.
##' @author Rich FitzJohn
##' @export
##' @import grid
treeGrob <- function(tree, direction="right", theta0=0,
                     name=NULL, gp=gpar(), vp=NULL) {
  direction <- match.arg(direction, tree_directions())

  xy <- plotting_prepare(tree)
  lim_t <- range(xy$time_rootward, xy$time_tipward, na.rm=TRUE)
  lim_s <- range(xy$spacing_min, xy$spacing_max) # NOTE: always [0,1]

  cvp <- scaling_viewport(lim_t, lim_s, direction, name="scaling")

  spacing_cols <- c("spacing_mid", "spacing_min", "spacing_max")
  if (direction == "circle") {
    xy[spacing_cols] <- spacing_to_angle(xy[spacing_cols],
                                         theta0=theta0, n=sum(xy$is_tip))
  } else if (direction == "semicircle") {
    xy[spacing_cols] <- spacing_to_angle(xy[spacing_cols], theta=pi)
  }

  branches <- tree_branchesGrob(xy, direction=direction,
                                name="branches", gp=gp, vp=cvp$name)

  ## NOTE: there is a double storage of information here; the tree
  ## contains all the information that the branches grob contains.
  ## But it's probably not desirable to pull that out of the tree
  ## every time we want to use it.
  gTree(tree=tree, direction=direction,
        children=gList(branches),
        childrenvp=cvp, name=name, gp=gp, vp=vp, cl="tree")
}

##' Add tip and node labels to a plotted tree.  These functions do not
##' do anything except for specify which \emph{type} of labels might
##' be added -- they need to be added to the tree with the addition
##' operator.
##'
##' @param type Either \code{tips} or \code{nodes} (or an abbreviation
##' of either).
##' @param offset A \code{unit} object describing offset in
##' the \emph{time} axis (positive is forward in time away from the
##' tip/node).
##' @param rot Rotation, relative to the \emph{time} axis.
##' @param name Name of the tip labels grob
##' @param gp Graphical parameters (a \code{gpar} object) for the
##' labels.
##' @export
tree_labels <- function(type, offset=unit(0.5, "lines"), rot=0,
                        name=NULL, gp=gpar()) {
  type  <- match.arg(type, c("tips", "nodes"))
  # TODO: with nonstandard names, tip and node labels become
  # unaddressable.  We'll need to get some semantics in there to allow
  # them to be restyled.  That's particularly important if we want to
  # add a handful of labels and then restyle them later...
  #
  # TODO: Check the gpar on entry here?  If it's non-scalar then
  # updating style later is hard.
  if (!is.null(name))
    stop("Not yet supported")
  name <- sprintf("%s_labels", sub("s$", "", type))
  object <- list(tips=type == "tips", nodes=type == "nodes",
                 offset=offset, rot=rot,
                 name=name, gp=gp)
  class(object) <- "tree_labels"
  object
}

##' @export
##' @rdname tree_labels
##' @param ... Arguments passed through to \code{tree_labels}.
tree_tip_labels <- function(...) {
  tree_labels("tips", ...)
}
##' @export
##' @rdname tree_labels
tree_node_labels <- function(...) {
  tree_labels("nodes", ...)
}

##' Apply graphics parameters to parts of a tree, varying based on
##' parent nodes.  The tree gets split into different regimes; there
##' is a base regime starting at the root, and then a series of
##' regimes painted onto the tree using the MEDUSA algorithm.  These
##' regimes can be nested.
##'
##' Generally, use the wrapper functions (\code{tree_style_branches},
##' \code{tree_style_tip_labels}, \code{tree_style_node_labels}) but the
##' low-level \code{style_thing} allows changing graphical parameters
##' of any member of the tree grob with a \code{labels} member.
##'
##' @title Style Tree By Node
##' @param what The child member of \code{tree_grob} to style.  At
##' present, values of "branches", "tip_labels" and "node_labels" are
##' supported, but any child member of \code{tree_grob} could be used
##' here.  A vector of names is allowed.
##' @param ... Named graphical parameters.  E.g., pass in
##' \code{node5=gpar(col="red")} will colour all descendents of "node5"
##' red.
##' @param base Base graphical parameters (by default the style is the
##' \code{gpar()}, but this will change).
##' @export
##' @rdname style
tree_style <- function(what, ..., base=NULL) {
  # TODO: Potential issue: a node called 'base' cannot be set.  Deal
  # with this by changing base to .base, perhaps
  targets <- list(...)
  if (length(targets) == 0)
    names(targets) <- character(0) # corner case.
  if (is.null(names(targets)) || any(names(targets) == ""))
    stop("Targets must be named")
  # TODO: This means that check_gpar is happening twice (once here and
  # once in combine_gpar()).  Not sure if that is actually a problem
  # though.
  targets <- lapply(targets, check_gpar)
  base <- check_gpar(base)
  object <- list(what=what, targets=targets, base=base)
  class(object) <- "tree_style"
  object
}

##' @export
##' @rdname style
tree_style_branches <- function(..., base=NULL) {
  tree_style("branches", ..., base=base)
}
##' @export
##' @rdname style
tree_style_tip_labels <- function(..., base=NULL) {
  tree_style("tip_labels", ..., base=base)
}
##' @export
##' @rdname style
tree_style_node_labels <- function(..., base=NULL) {
  tree_style("node_labels", ..., base=base)
}

## Lower level: Grobs

## Tree branches (not stored as separate time and spacing grobs
## because the details of how those are drawn may change).
tree_branchesGrob <- function(xy, direction,
                              name=NULL, gp=gpar(), vp=NULL) {
  grob(label=rownames(xy),
       time_tipward=xy$time_tipward, time_rootward=xy$time_rootward,
       spacing_min=xy$spacing_min,   spacing_max=xy$spacing_max,
       spacing_mid=xy$spacing_mid,
       is_tip=xy$is_tip,
       direction=direction, name=name, gp=gp, vp=vp,
       cl="tree_branches")
}

## Labels
##
## This is basically the same as a textGrob, but the units are in
## terms of spacing and time.
##
## Rotation is specified relative to the time axis (so that a value of
## 0 runs along the time axis).  This is butchered a little by circle
## plots so that the text is readable by flipping text around if it's
## approaching being upsidedown.
##
## Justification is not specifiable at the moment either, with
## apparent left justification and vertical centering being the only
## option.
tree_labelGrob <- function(label, t, s, direction, rot=0,
                           name=NULL, gp=gpar(), vp=NULL) {
  if (!is.numeric(s))
    stop("s must be numeric")
  if (!is.unit(t))
    stop("t must be a unit")
  grob(label=label, t=t, s=s, direction=direction, rot=rot,
       name=name, gp=gp, vp=vp, cl="tree_label")
}

## Low level: drawDetails methods to draw grobs

##' @S3method drawDetails tree_branches
drawDetails.tree_branches <- function(x, recording=TRUE) {
  if (x$direction %in% c("circle", "semicircle")) {
    # time
    grid.ray(native(x$time_rootward), native(x$time_tipward),
             x$spacing_mid, gp=x$gp)
    # spacing
    grid.arc(native(x$time_tipward), x$spacing_min,
             x$spacing_max, gp=x$gp)
  } else if (x$direction %in% c("left", "right")) {
    # time
    grid.segments(x$time_rootward, x$spacing_mid,
                  x$time_tipward, x$spacing_mid,
                  gp=x$gp, default.units="native")
    # spacing
    grid.segments(x$time_tipward, x$spacing_min,
                  x$time_tipward, x$spacing_max,
                  gp=x$gp, default.units="native")
  } else {
    # time:
    grid.segments(x$spacing_mid, x$time_rootward,
                  x$spacing_mid, x$time_tipward,
                  gp=x$gp, default.units="native")
    # spacing
    grid.segments(x$spacing_min, x$time_tipward,
                  x$spacing_max, x$time_tipward,
                  gp=x$gp, default.units="native")
  }
}

##' @S3method drawDetails tree_label
drawDetails.tree_label <- function(x, recording=TRUE) {
  if (x$direction %in% c("circle", "semicircle")) {
    rot <- x$rot + to_degrees(x$s)
    rot <- rot %% 360
    i <- rot > 90 & rot < 270
    rot[i] <- (rot[i] + 180) %% 360

    hjust <- rep_len(0, length(rot))
    hjust[i] <- 1

    xx <- polar_x(x$t, x$s)
    yy <- polar_y(x$t, x$s)
  } else {
    if (x$direction %in% c("left", "right")) {
      xx    <- x$t
      yy    <- x$s
      rot   <- 0
    } else {
      xx    <- x$s
      yy    <- x$t
      rot   <- 90
    }
    hjust <- if (x$direction %in% c("left", "down")) 1 else 0
  }
  vjust <- 0.5

  ## First line debugs alignment.
  # grid.points(xx, yy, gp=gpar(col="#ff000055"), pch=3)
  grid.text(x$label, xx, yy, hjust=hjust, vjust=vjust, rot=rot, gp=x$gp)
}

## Actually plot the tree
##' @S3method print tree
print.tree <- function(x, newpage=TRUE, vp=NULL, ...) {
  if (newpage)
    grid.newpage()
  if (!is.null(vp)) {
    pushViewport(vp)
    on.exit(upViewport())
  }
  grid.draw(x)
  invisible(x)
}
