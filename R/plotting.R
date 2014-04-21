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
##' low-level \code{tree_style} allows changing graphical parameters
##' of any member of the tree grob with a \code{labels} member.
##'
##' @title Style Tree By Node
##' @param class The class of a child member of \code{tree_grob} to
##' style. This is passed through to \code{\link{tree_match}} as
##' \code{class}, so anything that would be appropriate there is good
##' here (e.g., \code{tree_branches}, \code{tree_labels},
##' \code{tree_braces}.
##' @param ... Named graphical parameters.  E.g., pass in
##' \code{node5=gpar(col="red")} will colour all descendents of "node5"
##' red.
##' @param targets A named list of targets, each element of which is a
##' graphical parameters object.  Roughly equivalent to `list(...)`.
##' If this is provided, then `...` must be empty.  This form might be
##' better for programmatic use.  The `...` style might end up being
##' unnecessary, even.
##' @param descendants Apply styling to all descendants?  The default
##' is TRUE, using the MEDUSA algorithm to do this.  But specifying
##' \code{descendants=FALSE} allows styling of a single branch, node
##' label, etc.  For tips this has no effect as they have no
##' descendants!
##' @param base Base graphical parameters (by default the style is the
##' \code{gpar()}, but this will change).
##' @param name Passed through to \code{tree_match} - useful to
##' distinguish between multiple child members with the same class
##' (such as the tip and node labels).
##' @export
##' @rdname style
tree_style <- function(class, ..., targets=NULL,
                       descendants=TRUE, base=NULL, name=NULL) {
  if (is.null(targets)) {
    # TODO: Potential issue: a node called 'base' cannot be set.  Deal
    # with this by changing base to .base, perhaps
    targets <- list(...)
  } else {
    if (length(list(...))) {
      stop("Cannot specify both 'targets' and targets in '...'")
    }
    assert_list(targets)
  }
  # NOTE: This little hack is needed for classify -- basically the
  # list must *always* be named even when empty.
  if (length(targets) == 0) {
    names(targets) <- character(0)
  }
  assert_named(targets)
  assert_scalar(descendants)
  # TODO: This means that check_gpar is happening twice (once here and
  # once in combine_gpar()).  Not sure if that is actually a problem
  # though.
  targets <- lapply(targets, check_gpar)
  base <- check_gpar(base)
  object <- list(class=class, name=name, targets=targets, base=base,
                 descendants=descendants)
  class(object) <- "tree_style"
  object
}

##' @export
##' @rdname style
tree_style_branches <- function(..., base=NULL) {
  tree_style("tree_branches", ..., base=base)
}
##' @export
##' @rdname style
tree_style_tip_labels <- function(..., base=NULL) {
  tree_style("tree_labels", ..., base=base, name="tip_labels")
}
##' @export
##' @rdname style
tree_style_node_labels <- function(..., base=NULL) {
  tree_style("tree_labels", ..., base=base, name="node_labels")
}
##' @export
##' @rdname style
tree_style_brace <- function(..., base=NULL, name=NULL) {
  tree_style("tree_braces", ..., base=base, name=name)
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
##'
##' There are two basic types of images that we might want to add:
##' raster images (e.g., read by \code{readPNG} and plotted with
##' \code{grid.raster}) and vector images (through a fairly torturous
##' process).
##'
##' @title Add Image To Plotted Tree
##' @param images A named list of images; the name corresponds to the
##' tip or node that it will be associated with.  The actual images
##' can be raster images (produced by \code{as.raster}) with it), a
##' \code{nativeRaster} object or a matrix or array that
##' \code{as.raster} can handle, or they can be vector images (class
##' \code{Picture}, using the \code{grImport} package).  The list may
##' contain a mix of different types.
##' @param offset Offset, in the time dimension.  Watch out for
##' tip/node labels (this will happily draw on top of the labels).
##' @param rot Rotation of the image.  May be a scalar or vector of
##' the same length as \code{images}.
##' @param width Width of the image, before rotation.  Because
##' rotation may eventually happen in two places this is potentially
##' confusing.  Don't use "native" units unless you want unpredictable
##' results.  Ironically, that is the default.  May be a scalar or a
##' vector of the same length as \code{images}.
##' @param name Name to give the image within the tree
##' @param gp Graphical parameters.  According to the help for
##' \code{grid.raster} all parameters will be ignored, including
##' \code{alpha}, so this has no effect here.  In future versions,
##' vector images may allow use of some of these.
##' @author Rich FitzJohn
##' @export
tree_images <- function(images, offset=unit(0.5, "lines"),
                        rot=0, width=unit(1, "native"),
                        name=NULL, gp=gpar()) {
  assert_list(images)
  assert_named(images)
  images[] <- lapply(images, image_grob)

  width <- recycle_simple(width, length(images))
  rot   <- recycle_simple(rot,   length(images))

  tree_objects(images, offset=offset, rot=rot, width=width,
               name=name, gp=gp, class="tree_images")
}

## More plubmbing.  I think that for this one to work we have to
## convert object into a grob, and then everything else will just
## work!  If that's the case then this will probably never be
## exported.
tree_objects <- function(objects, offset=unit(0.5, "lines"),
                         rot=0, width=unit(0.1, "npc"),
                         name=NULL, gp=gpar(),
                         class=character(0)) {
  assert_list(objects)
  assert_named(objects)
  lapply(objects, assert_grob)

  ## All of these might change
  assert_scalar(offset)
  assert_length(rot,   length(objects))
  assert_length(width, length(objects))

  assert_unit(offset)
  assert_number(rot)
  assert_unit(width)

  object <- list(objects=objects, offset=offset, rot=rot,
                 width=width, name=name, gp=gp)
  class(object) <- c(class, "tree_objects")
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
tree_braces <- function(label, offset=unit(0.5, "lines"),
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
  # that tree_style will work correctly.

  object <- list(label=label, offset=offset, alignment=alignment,
                 name=name, gp=gp)
  class(object) <- "tree_braces"
  object
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
##' @param warn_no_match Give a warning if no match is found.
##' @return A list of \code{gPath} objects that can be used to
##' reference children \emph{relative to} \code{tree_grob}
##' @author Rich FitzJohn
##' @export
tree_match <- function(tree_grob, class=NULL, name=NULL,
                       warn_no_match=TRUE) {
  if (is.null(class) && is.null(name)) {
    stop("At least one of class and name must be given (and not NULL)")
  }
  if (is.null(class)) {
    ok.class <- rep(TRUE, length.out=length(tree_grob$children))
  } else {
    assert_scalar(class)
    ok.class <- sapply(tree_grob$children, inherits, class)
  }
  if (is.null(name)) {
    ok.name <- rep(TRUE, length.out=length(tree_grob$children))
  } else {
    assert_scalar(name)
    ok.name <- names(tree_grob$children) == name
  }

  kids <- lapply(names(tree_grob$children)[ok.name & ok.class], gPath)
  if (warn_no_match && length(kids) == 0) {
    # Future proofing for matching multiple things:
    f <- function(x) if (is.null(x)) "NULL" else paste(x, collapse=", ")
    warning(sprintf("Did not find any matches with class %s, name %s",
                    f(class), f(name)))
  }
  kids
}
