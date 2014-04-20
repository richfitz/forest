## Definitions of the drawDetails methods -- this is how things
## actually get drawn.
##
## Unlike other S3 methods in the package that are used only
## internally, these need registering in NAMESPACE so that grid can
## find them.

##' @S3method drawDetails tree_branches
drawDetails.tree_branches <- function(x, recording=TRUE) {
  tree_segments_time(x$spacing_mid,
                     native(x$time_rootward), native(x$time_tipward),
                     x$direction, gp=x$gp)
  tree_segments_spacing(x$spacing_min, x$spacing_max,
                        native(x$time_tipward),
                        x$direction, gp=x$gp)
}

##' @S3method drawDetails tree_labels
drawDetails.tree_labels <- function(x, recording=TRUE) {
  loc <- tree_location_resolve(x, rotate_to_time=TRUE)

  ## First line debugs alignment.
  # grid.points(xx, yy, gp=gpar(col="#ff000055"), pch=3)
  grid.text(x$label, loc$x, loc$y, hjust=loc$hjust, vjust=loc$vjust,
            rot=loc$rot, gp=x$gp)
}

## Actually nailing down width here is going to be *really* hard to
## get right with rotation.  Think very very hard about this.  For
## now, assuming width instead, which always has a meaning.  However,
## it won't have a well behaved "native" meaning.  Not sure if that's
## bad though.  Width is nice because it will be dependent on the
## object and not the tree, and will withstand rotation.  It's only
## going to be peculiar when rotation comes in.

##' @S3method drawDetails tree_braces
drawDetails.tree_braces <- function(x, recording=TRUE) {
  tree_segments_spacing(x$s_min, x$s_max, x$t, x$direction, gp=x$gp)
  # This is where a label would go if we knew what it would say.
}

##' @S3method drawDetails tree_object
drawDetails.tree_object <- function(x, recording=TRUE) {
  loc <- tree_location_resolve(x, rotate_to_time=FALSE)
  w <- x$width
  h <- w * (1 / aspect_ratio(x$object))
  vp <- viewport(x=loc$x, y=loc$y,  width=w, height=h,
                 angle=loc$rot, just=c(loc$hjust, loc$vjust))
  pushViewport(vp)
  on.exit(popViewport())
  grid.draw(x$object)
}
