\name{tree_image}
\alias{tree_image}
\title{Add Image To Plotted Tree}
\usage{
tree_image(image, label, offset = unit(0.5, "lines"), rot = 0,
  size = unit(1, "native"), name = NULL, gp = gpar())
}
\arguments{
  \item{image}{A raster object, from \code{readPNG} or
  \code{readJPEG} most likely.  Both "native" and
  array-based raster objects are supported.  See
  \code{grid.raster} for scope and limitations.}

  \item{label}{The label indicating the tip or node to
  associate the image with.  This may change soon!}

  \item{offset}{Offset, in the time dimension.  Watch out
  for tip/node labels (this will happily draw on top of the
  labels).}

  \item{rot}{Rotation of the image.  Currently ignored.}

  \item{size}{Size of the image.  This is hard to nail down
  because different tree orientations differ in which
  dimension it will be convenient to size the image.  For
  now it's the width, but that will also change.}

  \item{name}{Name to give the image within the tree (may
  change soon if we depend on this)}

  \item{gp}{Graphical parameters.  I don't think any of
  these are immediately useful, but I could be wrong.}
}
\description{
Add images to the tree.
}
\details{
Under heavy development at the moment, documentation
pending.
}
\author{
Rich FitzJohn
}

