## Utilities for working with grid graphics.

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

## Shorthand and easier to read.
native <- function(x)
  unit(x, "native")

## Grid tends to use degrees rather than radians
to_degrees <- function(x)
  x / pi * 180

##' Combine a list of graphical parameters.
##'
##' The idea is this; sometimes it makes sense to think of different
##' regions of a tree, or different lines, words, etc having different
##' graphical parameters, and we'd like to set them using the
##' \code{gpar()} function.  Something like
##' `list(base=gpar(col="black"), Asteraceae=gpar(col="yellow"))` and
##' then have these expanded out to a single \code{gpar} object with
##' the \code{col} element appropriately vectorised, so that the
##' branches, tip labels, node labels, etc for Asteraceae will end up
##' yellow while everything else remains black.
##'
##' This should work for any number of parameters, so that if
##' \code{lwd} is present in one list and not in the other, when
##' everything gets vectorised out, \code{lwd} is going to be needed
##' in both.  That means that we need to look up what the appropriate
##' \emph{default} value of \code{lwd} is going to be.  We do that by
##' using \code{get.gpar()} (if this is used during a
##' \code{drawDetails} method then this will behave most predictably,
##' otherwise it will probably end up with the grid defaults).
##'
##' Note that if no graphics window is open, this will open one.
##'
##' The first element of the list is considered to be the \emph{base}
##' parameters.
##'
##' This function does not do validation of parameters, or that we
##' have even got \code{gpar} objects at this point.  Also needing
##' validation: each element is a scalar.
##' @title Combine Graphical Parameters
##' @author Rich FitzJohn
##' @param list An R list (not a \code{gpar} list), each element of
##' which is a \code{gpar} list).  The first element is taken as the
##' "base" parameters.
##' @param index Integer index indicating how the parameters should be
##' expanded.
combine_gpars <- function(list, index) {
  ## TODO: If something is invariant, then record it as a scalar;
  ## don't expand out.
  ##
  ## TODO: This function is too weird.  Too hard to reason about.  I
  ## may have the wrong end of the stick here.
  if (length(list) < 1)
    stop("Need at least a base set of graphics parameters")
  if (min(index) < 1 || max(index) > length(list))
    stop("Invalid indices in index")
  list <- lapply(list, check_gpar)
  if (length(list) == 1)
    return(list[[1]])

  keys <- unique(unlist(lapply(list, names)))

  base <- list[[1]]
  base <- modifyList(base, get.gpar(setdiff(keys, names(base))))

  list.expanded <- lapply(list, function(x) modifyList(base, x))

  ret <- lapply(keys, function(k)
                unname(sapply(list.expanded, function(el) el[[k]])[index]))
  names(ret) <- keys
  class(ret) <- "gpar"
  ret
}

## This checks that an object is a gpar (or if is NULL creates an
## empty gpar because that's what grid implicitly assumes).  It's used
## in combine_gpars() above, but I'm not sure if it's generally useful.
check_gpar <- function(gp) {
  if (is.null(gp))
    gp <- gpar()
  else if (!inherits(gp, "gpar"))
    stop("Argument is not a gpar or NULL")
  if (length(gp) > 1 && any(sapply(gp, length) != 1))
    stop("All elements of 'gp' must be scalar")
  gp
}

# How much *wider* a thing is than it's *height*.  So an object with
# width w and h has aspect ratio w/h, or (w/h):1
aspect_ratio <- function(object, ...) {
  UseMethod("aspect_ratio")
}

aspect_ratio.rastergrob <- function(object, ...) {
  ncol(object$raster) / nrow(object$raster)
}

aspect_ratio.picture <- function(object, ...) {
  xscale <- range(object$hull$x)
  yscale <- range(object$hull$y)
  diff(range(xscale)) / diff(range(yscale))
}

aspect_ratio.Picture <- function(object, ...) {
  xscale <- object@summary@xscale
  yscale <- object@summary@yscale
  diff(range(xscale)) / diff(range(yscale))
}

## Untested, but this might work OK for general grobs:
## aspect_ratio.grob <- function(object, ...) {
##   convertHeight(grobHeight(object), "cm", TRUE) /
##     convertWidth(grobWidth(object), "cm", TRUE)
## }
