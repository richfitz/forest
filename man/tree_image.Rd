\name{tree_image}
\alias{tree_image}
\title{Add Image To Plotted Tree}
\usage{
tree_image(image, label, offset = unit(0.5, "lines"), rot = 0,
  width = unit(1, "native"), name = NULL, gp = gpar())
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

  \item{rot}{Rotation of the image.}

  \item{width}{Width of the image, before rotation.
  Because rotation may eventually happen in two places this
  is potentially confusing.  Don't use "native" units
  unless you want unpredictable results.}

  \item{name}{Name to give the image within the tree}

  \item{gp}{Graphical parameters.  According to the help
  for \code{grid.raster} all parameters will be ignored,
  including \code{alpha}, so this has no effect here.}
}
\description{
Add images to the tree.
}
\details{
Under heavy development at the moment, documentation
pending.

There are two basic types of images that we might want to
add: raster images (e.g., read by \code{readPNG} and
plotted with \code{grid.raster}) and vector images (through
a fairly torturous process).
}
\author{
Rich FitzJohn
}

