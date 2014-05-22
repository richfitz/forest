##' Flip an image along its horizontal or vertical axis.
##'
##' The picture can be a raster image, an array (that will later be
##' converted to raster), or a Picture object (vector graphics).  Note
##' that nativeRaster objects cannot (yet) be flipped.
##' @title Flip An Image
##' @param x A picture (see details)
##' @param horizontal Flip in the horizontal dimension (default is TRUE)
##' @param vertical Flip in the vertical dimension (default is FALSE)
##' @param ... Additional arguments currently ignored
##' @author Rich FitzJohn
##' @export
flip <- function(x, horizontal=TRUE, vertical=FALSE, ...) {
  UseMethod("flip")
}

##' @export
flip.Picture <- function(x, horizontal=TRUE, vertical=FALSE, ...) {
  tr <- function(path) {
    if (horizontal) {
      xscale <- x@summary@xscale
      path@x <- xscale[[2]] - path@x + xscale[[1]]
    }
    if (vertical) {
      yscale <- x@summary@yscale
      path@y <- yscale[[2]] - path@y + yscale[[1]]
    }
    path
  }

  x@paths[] <- lapply(x@paths, tr)
  x
}

# NOTE: the ncol(x):1 and nrow(x):1 calls are dangerous for zero-sized
# images, but they're going to be poorly behaved everywhere else
# anyway.  I've attempted to catch this condition, but it should be
# rare.
#
# NOTE: An alternative way would be three conditionals (h, v, h+v)
# with the h+v case x[nrow(x):1,ncol(x):1,,drop=FALSE], but that adds
# some complication.
#
# NOTE: plain matrices would also work here.
##' @export
flip.array <- function(x, horizontal=TRUE, vertical=FALSE, ...) {
  if (length(dim(x)) != 3L) {
    stop("Invalid size array")
  }
  if (any(dim(x) == 0)) {
    stop("Image has one dimension zero -- cannot flip")
  }
  if (horizontal) {
    x <- x[,ncol(x):1,,drop=FALSE]
  }
  if (vertical) {
    x <- x[nrow(x):1,,,drop=FALSE]
  }
  x
}

##' @export
flip.matrix <- function(x, horizontal=TRUE, vertical=FALSE, ...) {
  dim(x) <- c(dim(x), 1L)
  # Might cause issues with rasters of 1 column/row
  flip.array(x, horizontal, vertical, ...)[,,1L]
}

##' @export
flip.raster <- function(x, horizontal=TRUE, vertical=FALSE, ...) {
  # Issue here is that rasters are stored in row order, and there
  # doesn't seem to be much in the way of [<- support.  So this pile
  # of hacks seems to work.
  x <- as.matrix(x)
  ret <- flip.matrix(x, horizontal, vertical, ...)
  ret[] <- t(ret)
  class(ret) <- "raster"
  ret
}

# Something else needs to be done for nativeRaster, but not sure
# what.  There's no huge hassle here though.

##' Colour all paths in a vector image (class \code{Picture}) with a
##' single colour.  This is probably most useful for straight
##' silhouettes, rather than those with multiple colours.
##'
##' May change to allow matching on source colour in the future.
##' @title Colour a Vector Picture
##' @param picture A \code{Picture} object
##' @param col Colour to cange paths to
##' @author Rich FitzJohn
##' @export
colour_picture <- function(picture, col) {
  assert_picture(picture)
  for (i in seq_along(picture@paths))
    picture@paths[[i]]@rgb <- col
  picture
}

##' Compute aspect ratio of images (and potentially/eventually other
##' graphical objects).
##'
##' This is how much \emph{wider} a thing is than its height.  So an
##' object with width "w" and height "h" has  aspect ratio of w/h,or
##' (w/h):1.  If it is wider than it is high, then the aspect ratio is
##' greater than one.
##'
##' @title Compute Aspect Ratio
##' @param object Object to compute as
##' @param ... Additional arguments to methods, all ignored
##' @author Rich FitzJohn
##' @export
aspect_ratio <- function(object, ...) {
  UseMethod("aspect_ratio")
}

##' @export
aspect_ratio.raster <- function(object, ...) {
  ncol(object) / nrow(object)
}
##' @export
aspect_ratio.array        <- aspect_ratio.raster
##' @export
aspect_ratio.matrix       <- aspect_ratio.raster
##' @export
aspect_ratio.nativeRaster <- aspect_ratio.raster

##' @export
aspect_ratio.Picture <- function(object, ...) {
  xscale <- object@summary@xscale
  yscale <- object@summary@yscale
  diff(range(xscale)) / diff(range(yscale))
}

##' @export
aspect_ratio.rastergrob <- function(object, ...) {
  ncol(object$raster) / nrow(object$raster)
}

# This is what pictureGrob() returns
##' @export
aspect_ratio.picture <- function(object, ...) {
  xscale <- range(object$hull$x)
  yscale <- range(object$hull$y)
  diff(range(xscale)) / diff(range(yscale))
}

## Untested, but this might work OK for general grobs, provided that
## it is done during drawDetails
## aspect_ratio.grob <- function(object, ...) {
##   convertHeight(grobHeight(object), "cm", TRUE) /
##     convertWidth(grobWidth(object), "cm", TRUE)
## }

## This might also want breaking up into S3 methods, so that it is
## open for extension.  But that won't be obvious to end use.
image_grob <- function(object) {
  if (inherits(object, c("raster", "nativeRaster"))) {
    grob <- rasterGrob(object)
  } else if (is.array(object)) {
    grob <- rasterGrob(as.raster(object))
  } else if (inherits(object, "Picture")) {
    grob <- grImport::pictureGrob(object)
  } else {
    stop("Not something I recognise as an image")
  }
}
