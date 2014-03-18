## Lower level functions that are not exported.  Everything in this
## file is subject to complete change.
tree_directions <- function() {
  c("right", "left", "up", "down", "circle", "semicircle")
}

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

spacing_to_angle <- function(s, theta0=0, theta=pi * 2 * (n-1) / n, n) {
  theta1 <- theta0 + theta
  theta0 + s * (theta1 - theta0)
}
