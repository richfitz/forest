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

##' @S3method flip Picture
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
##' @S3method flip array
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

##' @S3method flip matrix
flip.matrix <- function(x, horizontal=TRUE, vertical=FALSE, ...) {
  dim(x) <- c(dim(x), 1L)
  # Might cause issues with rasters of 1 column/row
  flip.array(x, horizontal, vertical, ...)[,,1L]
}

##' @S3method flip raster
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
