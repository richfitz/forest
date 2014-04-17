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
##' Specifying a nonzero value for non-circle plots will generate an
##' error (may eventually be softened to a warning).
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

  # TODO: The range calculations could be restricted to within the
  # scaling_viewport code, perhaps?
  lim_t <- range(xy$time_rootward, xy$time_tipward, na.rm=TRUE)
  lim_s <- range(xy$spacing_min, xy$spacing_max) # NOTE: always [0,1]
  cvp <- scaling_viewport(lim_t, lim_s, direction, name="scaling")

  spacing_info <- spacing_info(sum(xy$is_tip), direction)

  if (theta0 != 0 && direction != "circle") {
    stop("theta0 argument only valid for circle plots (at present)")
  }

  if (direction %in% c("circle", "semicircle")) {
    spacing_cols <- c("spacing_mid", "spacing_min", "spacing_max")
    xy[spacing_cols] <- theta0 + xy[spacing_cols] * spacing_info$size
  }

  branches <- tree_branchesGrob(xy, direction=direction,
                                name="branches", gp=gp, vp=cvp$name)

  ## NOTE: there is a double storage of information here; the tree
  ## contains all the information that the branches grob contains.
  ## But it's probably not desirable to pull that out of the tree
  ## every time we want to use it.
  gTree(tree=tree, direction=direction, spacing_info=spacing_info,
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

## TODO: For now just using label, but that should naturally expand
## out to providing lower level t/s pairs.  That's what this is going
## to use anyway.
##
## TODO: In the case where have labels in the tree already should this
## detect that and add the strwidth of the label to the offset?  That
## would actually be very easy.
##
## TODO: When providing multiple images, how do we provide them?  List
## I guess.
##
## TODO: Is it ever useful to provide filenames?  Probably not.
##
## TODO: Handle both vector and bitmap images through the same
## interface.  Users should not care.
##
## TODO: Order of arguments?  Probably picture first.  Others should
## reflect tree_labels (especially rot)
##
## TODO: Recycling of pictures?  Probably never a good idea.
##
## TODO: Default sizing of images?  Because the size dimension is only
## defined as 0..1 it varies depending on how many species are
## plotted.  This is even worse for circle trees, where that unit will
## vary depending on the time axis anyway.  Some sort of concept of
## the between-branch-space might be useful to have here.

##' Add images to the tree.
##'
##' Under heavy development at the moment, documentation pending.
##' @title Add Image To Plotted Tree
##' @param image A raster object, from \code{readPNG} or
##' \code{readJPEG} most likely.  Both "native" and array-based raster
##' objects are supported.  See \code{grid.raster} for scope and
##' limitations.
##' @param label The label indicating the tip or node to associate the
##' image with.  This may change soon!
##' @param offset Offset, in the time dimension.  Watch out for
##' tip/node labels (this will happily draw on top of the labels).
##' @param rot Rotation of the image.  Currently ignored.
##' @param size Size of the image.  This is hard to nail down because
##' different tree orientations differ in which dimension it will be
##' convenient to size the image.  For now it's the width, but that
##' will also change.
##' @param name Name to give the image within the tree (may change
##' soon if we depend on this)
##' @param gp Graphical parameters.  I don't think any of these are
##' immediately useful, but I could be wrong.
##' @author Rich FitzJohn
##' @export
tree_image <- function(image, label, offset=unit(0.5, "lines"),
                       rot=0, size=unit(1, "native"),
                       name=NULL, gp=gpar()) {
  ## TODO: Thinking about this, this would actually generalise nicely
  ## to all grobs that we might want to add.  So pie charts at nodes,
  ## things like that.  The requirements would probably be that we
  ## need to be able to compute sizes and aspect ratios meaningfully.
  ## It may or may not make sense to retrofit the existing tip labels
  ## into the same structure, and that pretty much depends on how the
  ## arguments vectorise.  For now I'm ignoring that issue.

  if (!inherits(image, "nativeRaster")) {
    # TODO: This is ugly, but as.raster("foo") does not actually fail,
    # surprisingly.  This check is needed so that filenames cause
    # errors.
    if (is.array(image)) {
      image <- as.raster(image)
    } else {
      stop("Not something that can be converted into a raster")
    }
  }

  ## All of these might change
  if (length(label) != 1)
    stop("Need a scalar label at the moment")
  if (length(offset) != 1)
    stop("Need a scalar offset at the moment")
  if (length(rot) != 1)
    stop("Need a scalar offset at the moment")
  if (length(size) != 1)
    stop("Need a scalar size at the moment")

  # Other checking that is more likely to be permanent
  if (!is.unit(offset))
    stop("offset must be a unit")
  if (!(is.numeric(rot) || is.integer(rot)) || is.na(rot))
    stop("rot must be numeric")
  if (!is.unit(size))
    stop("size must be a unit")

  object <- list(image=image, label=label, offset=offset, rot=rot,
                 size=size, name=name, gp=gp)
  class(object) <- "tree_image"
  object
}

##' Put a brace around a single clade.
##'
##' This is going to change, but exists as a holding place so that I
##' can develop the lower level code.  Eventually this is going to be
##' subsumed within things like the taxonomic alignment, but
##' \emph{that} will probably work by using a series of clades anyway,
##' so we're unlikely to be outrageously off.
##'
##' Eventually this has to handle a brace (type [straight line, square
##' bracket, curly bracket], offset from tree, graphical styling
##' [colour, line type, line thickness]) and a label for the brace
##' (position [centred, top, bottom], graphical styling [colour, font,
##' etc], text alignment, offset from brace).  So this function will
##' break into pieces as development proceeds.  For now, starting with
##' the simplest cases and moving forward.
##' @title Add Brace To Plotted Tree
##' @param label Label in the tree to attach the brace to.  The
##' brace will span all descendents of the node (if internal) or just
##' the single species (if terminal).  \emph{NB} this is \emph{not}
##' the label that will be eventually drawn next to the brace.
##' @param offset A \code{unit} object describing offset in
##' the \emph{time} axis (positive is forward in time away from the
##' tip/node).
##' @param alignment Alignment of multiple braces, which has a visual
##' effect only on non-ultrametric trees.  Possible options are "none"
##' (the default; all braces are put 'offset' away from the longest
##' tip for each brace), "set" (all braces are put 'offset' away from
##' the longest tip among all tips spaned by braces in this set of
##' braces) and "global" (all braces are put 'offset' away from the
##' longest tip in the tree).
##' @param name Name for the brace (optional)
##' @param gp Graphical parameters (optional)
##' @author Rich FitzJohn
##' @export
tree_brace <- function(label, offset=unit(0.5, "lines"),
                       alignment="none",
                       name=NULL, gp=gpar()) {
  alignment <- match.arg(alignment, c("none", "set", "global"))
  # TODO: In circle tree, when theta0 > 0, check that we don't do mod
  # 2*pi because that will break working out where the beginning and
  # end points are for the contents of the clade.
  #
  # TODO: Going to need to work out how the labels are positioned to
  # nail the offset here.

  # All of these might change
  if (length(offset) != 1)
    stop("Need a scalar offset at the moment")

  # Other checking that is more likely to be permanent
  if (length(label) == 0 || !is.character(label))
    stop("label must be a non-empty character vector")
  if (!is.unit(offset))
    stop("offset must be a unit")

  # TODO (and also elsewhere): check that gp elements are scalar so
  # that style_thing will work correctly.

  object <- list(label=label, offset=offset, alignment=alignment,
                 name=name, gp=gp)
  class(object) <- "tree_brace"
  object
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

tree_imageGrob <- function(image, t, s, direction, size, rot=0,
                           name=NULL, gp=gpar(), vp=NULL) {
  if (!is.numeric(s))
    stop("s must be numeric")
  if (!is.unit(t))
    stop("t must be a unit")
  if (!is.unit(size))
    stop("size must be a unit")
  grob(image=image, t=t, s=s, direction=direction, size=size, rot=rot,
       name=name, gp=gp, vp=vp, cl="tree_image")
}

tree_braceGrob <- function(label, t, s_min, s_max, direction,
                           name=NULL, gp=gpar(), vp=NULL) {
  if (!is.numeric(s_min) || !is.numeric(s_max))
    stop("s_min and s_max must be numeric")
  if (!is.unit(t))
    stop("t must be a unit")
  grob(label=label, t=t, s_min=s_min, s_max=s_max, direction=direction,
       name=name, gp=gp, vp=vp, cl="tree_brace")
}

## Low level: drawDetails methods to draw grobs

##' @S3method drawDetails tree_branches
drawDetails.tree_branches <- function(x, recording=TRUE) {
  tree_segments_time(x$spacing_mid,
                     native(x$time_rootward), native(x$time_tipward),
                     x$direction, gp=x$gp)
  tree_segments_spacing(x$spacing_min, x$spacing_max,
                        native(x$time_tipward),
                        x$direction, gp=x$gp)
}

##' @S3method drawDetails tree_label
drawDetails.tree_label <- function(x, recording=TRUE) {
  loc <- tree_location_resolve(x, rotate_to_time=TRUE)

  ## First line debugs alignment.
  # grid.points(xx, yy, gp=gpar(col="#ff000055"), pch=3)
  grid.text(x$label, loc$x, loc$y, hjust=loc$hjust, vjust=loc$vjust,
            rot=loc$rot, gp=x$gp)
}

##' @S3method drawDetails tree_image
drawDetails.tree_image <- function(x, recording=TRUE) {
  loc <- tree_location_resolve(x, rotate_to_time=FALSE)
  # TODO: rot only works here via a viewport, so we'll ignore.
  #
  # TODO: for up/down, size should be on the width dimension, or
  # convert the size -> width with convertWidth, etc.  We can work
  # that out when building the image grob, perhaps.
  grid.raster(x$image, loc$x, loc$y, hjust=loc$hjust, vjust=loc$vjust,
              height=x$size, gp=x$gp)
}

##' @S3method drawDetails tree_brace
drawDetails.tree_brace <- function(x, recording=TRUE) {
  tree_segments_spacing(x$s_min, x$s_max, x$t, x$direction, gp=x$gp)
  # This is where a label would go if we knew what it would say.
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

##' Match elements within a tree grob (generated by
##' \code{\link{treeGrob}}), using the name or class of the child
##' component.
##'
##' This matches elements that were added to the tree by the \code{+}
##' operator; such as \code{tree_grob + tree_node_labels()}.  A few
##' rules are useful here (and may change).
##'
##' All classes provided by \code{forest} are prefixed with
##' \code{tree_}, so elements provided as \code{class} will almost
##' always need this adding.  I might change things to automatically
##' add this in the future though, as the prefix might change.
##'
##' \code{tree_tip_labels} and \code{tree_node_labels} \emph{both}
##' produce objects with class \code{tree_labels}.
##' \code{tree_tip_labels} will always have the \emph{name} of
##' \code{tips} and \code{tree_node_labels} will always have the name
##' of \code{nodes}, so they can be addressed with \code{tree_match(.,
##' class="tree_labels", name="tips")}, for example.
##'
##' There can be only one set of branches and this always has the name
##' "branches".
##'
##' When both specified, \code{class} and \code{name} must \emph{both}
##' be satisified.
##'
##' @title Match Children in Tree Grob
##' @param tree_grob A grob created by \code{treeGrob}
##' @param class The class of the object to match.  Currently this can
##' be any class attribute (for cases with multiple inheritance) but
##' this might change soon to be only the primary class.  Also it only
##' makes sense to match things with a \code{tree_} prefix so that
##' requirement may also drop soon.
##' @param name The name of the object to match.  Some functions set
##' the name automatically for you (e.g., \code{tree_tip_labels}, but
##' others default to a \code{grid} provided name, which is varies
##' from use to use unless one is provided manually.
##' @return A list of \code{gPath} objects that can be used to
##' reference children \emph{relative to} \code{tree_grob}
##' @author Rich FitzJohn
##' @export
tree_match <- function(tree_grob, class=NULL, name=NULL) {
  if (is.null(class) && is.null(name)) {
    stop("At least one of class and name must be given (and not NULL)")
  }
  if (is.null(class)) {
    ok.class <- rep(TRUE, length.out=length(tree_grob$children))
  } else {
    check_scalar(class)
    ok.class <- sapply(tree_grob$children, inherits, class)
  }
  if (is.null(name)) {
    ok.name <- rep(TRUE, length.out=length(tree_grob$children))
  } else {
    check_scalar(name)
    ok.name <- names(tree_grob$children) == name
  }

  lapply(names(tree_grob$children)[ok.name & ok.class], gPath)
}
