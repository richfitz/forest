## NOTE: Coding standards here follow `package:grid` as much as
## possible; treeGrob, in camelCase, is a "tree graphical object" etc.

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
treeGrob <- function(tree, direction="right",
                     name=NULL, gp=NULL, vp=NULL) {
  direction <- match.arg(direction,
                         c("right", "left", "up", "down", "circle"))

  xy <- plotting_prepare(tree)
  lim_t <- range(xy$time_rootward, xy$time_tipward, na.rm=TRUE)
  lim_s <- range(xy$spacing_min, xy$spacing_max) # NOTE: always [0,1]

  cvp <- scaling_viewport(lim_t, lim_s, direction, name="scaling")

  ## Possibly rewrite spacing_to_angle to take all of xy?
  if (direction == "circle") {
    spacing_cols <- c("spacing_mid", "spacing_min", "spacing_max")
    xy[spacing_cols] <- spacing_to_angle(xy[spacing_cols], n=sum(xy$is_tip))
  }

  seg_t <- tree_seg_timeGrob(xy$time_rootward, xy$time_tipward,
                             xy$spacing_mid, label=rownames(xy),
                             is_tip=xy$is_tip, name="seg_t",
                             direction=direction, gp=gp, vp=cvp$name)
  seg_s <- tree_seg_spacingGrob(xy$spacing_min, xy$spacing_max,
                                xy$time_tipward, label=rownames(xy),
                                is_tip=xy$is_tip, name="seg_s",
                                direction=direction, gp=gp, vp=cvp$name)

  gTree(tree=tree, direction=direction,
        children=gList(seg_t, seg_s),
        childrenvp=cvp, name=name, gp=gp, vp=vp, cl="tree")
}

##' Add tip and node labels to a plotted tree.  The idea is to take a
##' tree that is created by \code{\link{treeGrob}} and add tip labels
##' into it.
##'
##' The interface will change here, and this function \emph{will}
##' change and probably disappear.  Eventually we're working towards a
##' \code{ggplot}-style \code{+} operators on tree graphical elements.
##'
##' @title Add Tip and Node Labels to a Plotted Tree
##' @param tree_grob A tree grob, created by \code{\link{treeGrob}}
##' @param ... Additional arguments passed through to underlying
##' functions (NB: interface may change!).  Useful things include
##' \code{offset}, which is a \code{unit} object describing offset in
##' the \emph{time} axis (positive is forward in time away from the
##' tip/node), \code{gp}, which is a graphical parameters list
##' (created by \code{gpar}; as with \code{\link{treeGrob}}, avoid
##' passing in vectors here, \code{rot}, which is the rotation
##' (relative to the time axis).
##' @author Rich FitzJohn
##' @export
add_tip_labels <- function(tree_grob, name="tip_labels", ...) {
  add_tree_labels_internal(tree_grob, tip=TRUE, node=FALSE, name=name,
                           ...)
}
##' @rdname add_tip_labels
##' @export
add_node_labels <- function(tree_grob, name="node_labels", ...) {
  add_tree_labels_internal(tree_grob, tip=FALSE, node=TRUE, name=name,
                           ...)
}

add_tree_labels <- function(tree_grob, label, t, s, rot=0,
                            name=NULL, gp=gpar()) {
  lab <- tree_labelGrob(label, t, s, tree_grob$direction, rot,
                        name=name, gp=gp, vp=tree_grob$childrenvp)
  addGrob(tree_grob, lab)
}

## This one pulls names and locations out of the tree object and runs
## creates a tree_labelGrob out of it.  It might be a level of
## redirection too far, but it avoids a bunch of repetition.
add_tree_labels_internal <- function(tree_grob,
                                     offset=unit(0.5, "lines"),
                                     tip=FALSE, node=FALSE, rot=0,
                                     name=NULL, gp=gpar()) {
  offset_t <- normalise_time(offset, tree_grob$direction)
  seg_t <- tree_grob$children$seg_t
  i <- (seg_t$is_tip & tip) | (!seg_t$is_tip & node)
  add_tree_labels(tree_grob, seg_t$label[i],
                  native(seg_t$t1[i]) + offset_t, seg_t$s[i], rot,
                  name=name, gp=gp)
}

## Lower level functions that are not exported:

## TODO: This is all far uglier than it wants to be, and can probably
## be done massively faster in native code.  Until things settle down
## though, leave it as this.
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

## I'm not sure that this is actually the best way of handling the
## spacing, really.  It might be better to put a function within the
## object so that we provide a *translation* and so the original
## spacing can be relied on.  Not sure.
spacing_to_angle <- function(s, theta0=0, theta1=pi * 2 * (n-1) / n, n) {
  theta0 + s * (theta1 - theta0)
}

## Lower level graphical functions that are not exported:

## Viewport that establishes the "native" scale.  For a circular plot
## this also sets the aspect to be 1 so that the circle does not
## become an ellipse.
scaling_viewport <- function(lim_t, lim_s, direction, ...) {
  if (direction == "circle") {
    lim <- c(-1, 1) * lim_t[2]
    viewport(xscale=lim, yscale=lim,
             width=unit(1, "snpc"), height=unit(1, "snpc"), ...)
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
## along spacing (spacing grobs)
tree_seg_timeGrob <- function(t0, t1, s, label, is_tip, direction,
                              name=NULL, gp=NULL, vp=NULL) {
  grob(t0=t0, t1=t1, s=s, label=label, is_tip=is_tip, direction=direction,
       name=name, gp=gp, vp=vp, cl="tree_seg_time")
}
tree_seg_spacingGrob <- function(s0, s1, t, label, is_tip, direction,
                                 name=NULL, gp=NULL, vp=NULL) {
  grob(s0=s0, s1=s1, t=t, label=label, is_tip=is_tip, direction=direction,
       name=name, gp=gp, vp=vp, cl="tree_seg_spacing")
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

##' @S3method drawDetails tree_seg_time
drawDetails.tree_seg_time <- function(x, recording=TRUE) {
  if (x$direction == "circle") {
    grid.ray(native(x$t0), native(x$t1), x$s)
  } else if (x$direction %in% c("left", "right")) {
    grid.segments(x$t0, x$s, x$t1, x$s, gp=x$gp, default.units="native")
  } else {
    grid.segments(x$s, x$t0, x$s, x$t1, gp=x$gp, default.units="native")
  }
}

##' @S3method drawDetails tree_seg_spacing
drawDetails.tree_seg_spacing <- function(x, recording=TRUE) {
  if (x$direction == "circle") {
    grid.arc(native(x$t), x$s0, x$s1)
  } else if (x$direction %in% c("left", "right")) {
    grid.segments(x$t, x$s0, x$t, x$s1, gp=x$gp, default.units="native")
  } else {
    grid.segments(x$s0, x$t, x$s1, x$t, gp=x$gp, default.units="native")
  }
}

##' @S3method drawDetails tree_label
drawDetails.tree_label <- function(x, recording=TRUE) {
  if (x$direction == "circle") {
    rot <- x$rot + to_degrees(x$s)
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
  grid.rect(gp=gpar(lty=3, col="grey"))
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
