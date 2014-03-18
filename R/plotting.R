## NOTE: Coding standards here follow `package:grid` as much as
## possible; treeGrob, in camelCase, is a "tree graphical object" etc.

## TODO: There is a mix of low level grid functions (e.g. grid.arc,
## grid.ray), utility functions (e.g. spacing_to_angle) in addition to
## the actually useful things in this file.  Reorganise.

## WARNING: Probably more than the rest of the package, the interface
## here will change massively as I work out what actually works
## nicely.

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

##' Add tip and node labels to a plotted tree.  The idea is to take a
##' tree that is created by \code{\link{treeGrob}} and add tip labels
##' into it.
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

add_tree_labels <- function(tree_grob, tree_labels) {
  direction <- tree_grob$direction
  offset_t <- normalise_time(tree_labels$offset, direction)
  branches <- tree_grob$children$branches
  i <- ( branches$is_tip & tree_labels$tip) |
       (!branches$is_tip & tree_labels$node)

  label <- branches$label[i]
  t <- native(branches$time_tipward[i]) + offset_t
  s <- branches$spacing_mid[i]

  lab <- tree_labelGrob(label, t, s, direction, tree_labels$rot,
                        name=tree_labels$name, gp=tree_labels$gp,
                        vp=tree_grob$childrenvp)
  addGrob(tree_grob, lab)
}

## Lower level functions that are not exported:

## TODO: This is all far uglier than it wants to be, and can probably
## be done massively faster in compiled code.  Until things settle
## down though, leave it as this.
plotting_prepare <- function(tree) {
  treeapply <- function(tr, f)
    lapply(drain_tree(tr), f)
  tp <- plotting_coordinates(tree)
  xy <- do.call(rbind, treeapply(tp, function(x) unlist(x$data)))
  rownames(xy) <- unlist(treeapply(tp, function(x) x$label))
  xy <- as.data.frame(xy)
  xy$is_tip <- as.logical(xy$is_tip)
  xy
}

spacing_to_angle <- function(s, theta0=0, theta=pi * 2 * (n-1) / n, n) {
  theta1 <- theta0 + theta
  theta0 + s * (theta1 - theta0)
}

## Lower level graphical functions that are not exported:

## Viewport that establishes the "native" scale.  For a circular plot
## this also sets the aspect to be 1 so that the circle does not
## become an ellipse.
scaling_viewport <- function(lim_t, lim_s, direction, ...) {
  if (direction %in% "circle") {
    lim <- c(-1, 1) * lim_t[2]
    viewport(xscale=lim, yscale=lim,
             width=unit(1, "snpc"), height=unit(1, "snpc"), ...)
  } else if (direction == "semicircle") {
    viewport(xscale=c(-1, 1) * lim_t[2], yscale=lim_t,
             width=unit(1, "snpc"), height=unit(0.5, "snpc"), ...)
  } else {
    if (direction %in% c("left", "right")) {
      xscale <- if (direction == "right") lim_t else rev(lim_t)
      yscale <- lim_s
    } else if (direction %in% c("up", "down")) {
      xscale <- lim_s
      yscale <- if (direction == "up")    lim_t else rev(lim_t)
    }
    viewport(xscale=xscale, yscale=yscale, ...)
  }
}

## The two types of tree component lines; along  time (time grobs) and
## along spacing (spacing grobs).  It might be better to store this as
## a gList, but that requires some care about storing different
## graphical parameters.
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


## Grid utilities:

## Warning: these must be used in a drawDetails method for things to
## accurately work across device resizing.
polar_x <- function(r, theta)
  convertWidth(r * cos(theta), "native")
polar_y <- function(r, theta)
  convertHeight(r * sin(theta), "native")

## convertWidth here would be insensitive to resize unless it is drawn
## during a drawDetails() method call.  Move this out to be a "ray"
## grob (and similarly with the arc grob).
##
## As it currently is, this *should* be fine while used only for the
## trees, but once we use these things more broadly within the tree
## plotting it will get annoying.
##
## TODO: Convert these to a rayGrob and arcGrob and have these be the
## draw methods of them.  No code that uses these should particularly
## care about this change, which is nice.
grid.ray <- function(r0, r1, theta, ...) {
  grid.segments(polar_x(r0, theta), polar_y(r0, theta),
                polar_x(r1, theta), polar_y(r1, theta),
                ...)
}

## See comments about grid.ray and redrawing, which apply here.
grid.arc <- function(r, theta0, theta1, ...) {
  theta <-  abs(theta1 - theta0)
  for (i in which(theta > 0 & convertWidth(r, "native", TRUE) > 0))
    grid.arc.xy(polar_x(r[i], theta0[i]), polar_y(r[i], theta0[i]),
                polar_x(r[i], theta1[i]), polar_y(r[i], theta1[i]),
                theta[i], ...)
}

## TODO: The 33 control points here is arbitrary, and is going to be
## higher than needed in some situations, lower in others.  Consider
## scaling against how much of the full circle theta runs?
grid.arc.xy <- function(x0, y0, x1, y1, theta=NULL, ncp=33, ...) {
  if (is.null(theta))
    theta <- atan2(y1 - y0, x1 - x0)
  grid.curve(x0, y0, x1, y1, square=FALSE,
             curvature=arcCurvature(to_degrees(theta)), ncp=ncp, ...)
}

## Utility things
to_degrees <- function(x)
  x / pi * 180

native <- function(x)
  unit(x, "native")

## NOTE: This is a temporary hack until I improve the handling of trees that
## plot the "wrong" way (i.e., left and down).
##
## NOTE: convertWidth(unit(1, "lines")) is *negative* when converted
## to a left tree (and similarly a down tree), but with unit "native"
## this does not seem to happen.  This is because of the reflected
## axis labels.  Tricky.  I don't really see a nice way of dealing
## with this; we want
##   unit(x, "native") + unit(1, "foo")
## to *increase* x always.
##
## This possibly means that I should not be reversing the scales?  I
## would not be surprised if we get badly caught by things like
## grid.picture() there?  Deferring sorting this out until I work out
## how this will interact with things like just/adj, etc.
normalise_time <- function(unit, direction) {
  if (!(direction %in% c("left", "down")))
    return(unit)
  if (inherits(unit, "unit.arithmetic")) {
    unit$arg1 <- normalise_time(unit$arg1)
    unit$arg2 <- normalise_time(unit$arg2)
    unit
  } else if (is.unit(unit)) {
    if (attr(unit, "unit") != "native")
      unit[] <- -unclass(unit)
    unit
  } else {
    stop("Invalid argument")
  }
}

tree_directions <- function() {
  c("right", "left", "up", "down", "circle", "semicircle")
}

## The ideas in this section borrow *very* heavily from ggplot2.
##' @S3method + tree
`+.tree` <- function(e1, e2) {
  add_tree(e1, e2)
}

## This is going to be better do do through partial application and S3
## methods, which will avoid the big mess of if/else statements and
## simultaneously be more extensible.  Once that's done, the two
## separate (+.tree and add_tree) can roll back together.
add_tree <- function(tg, object) {
  if (inherits(object, "tree_labels")) {
    add_tree_labels(tg, object)
  } else if (inherits(object, "tree_style")) {
    add_tree_style(tg, object)
  } else {
    stop("Can't add that sort of thing...")
  }
}

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

add_tree_style <- function(tree_grob, tree_style) {
  targets <- tree_style$targets
  base <- tree_style$base
  what <- tree_style$what
  cl <- classify(tree_grob$tree, names(targets)) + 1L

  for (w in what) {
    if (!(w %in% names(tree_grob$children)))
      stop("No child member ", w, " within this tree")
    thing <- tree_grob$children[[w]]
    if (!("label" %in% names(thing)))
      stop("Tree member ", w, " does not have a label member")
    base.w <- if (is.null(base)) thing$gp else base
    # TODO: It might be best here to delay this till the drawDetails
    # part, or at least retrigger base lookup.  That would not be hard
    # to do, and actually allow restyling a bit more easily
    # potentially.
    #
    # TODO: perhaps first look to the object's gpar?  Or would
    # deferring to drawing solve this too?
    tree_grob$children[[w]]$gp <-
      combine_gpars(c(list(base.w), targets), unname(cl[thing$label]))
  }

  # TODO: Here, and in add_tree_labels, or in add_tree, should the
  # return be invisible?
  tree_grob
}
