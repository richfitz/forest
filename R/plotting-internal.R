## Lower level functions that are not exported.  Everything in this
## file is subject to complete change.
tree_directions <- function() {
  c("right", "left", "up", "down", "circle", "semicircle")
}

## TODO: This is all far uglier than it wants to be, and can probably
## be done massively faster in compiled code.  Until things settle
## down though, leave it as this.
##
## TODO: The clade tree stuff is bolted on here with the last two
## arguments.  That's going to change once we get proper clade tree
## support.
plotting_prepare <- function(tree, n_taxa=NULL, p=0.5) {
  treeapply <- function(tr, f)
    lapply(drain_tree(tr), f)
  if (is.null(n_taxa)) {
    tp <- plotting_coordinates(tree)
  } else {
    tp <- plotting_coordinates_clade(tree, n_taxa, p)
  }
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

# Compute the separation between tips.  This is a slightly tricky
# thing.  For left/right/up/down and semicircle plots, the domain
# ([0,1] and [0,pi], respectively) is divided into n-1 sections.  For
# circular plots the domain ([0,2pi]) is divided into n sections.
#
# Clade trees will break this entirely because there is a separation
# between the number of tips and number of taxa, and the tips are of
# varying size, so picking a good looking number will take a bit of
# effort, and I will need to rethink this a little when I get there.
#
# NOTE: We might want a slightly larger gap to indicate where the
# "beginning" of the tree is for the circle tree, but for now it is
# worth one gap.
spacing_info <- function(n_tips, direction) {
  if (direction %in% c("left", "right", "down", "up")) {
    size <- 1
    gaps <- n_tips - 1
  } else if (direction == "circle") {
    size <- 2 * pi * (n_tips - 1) / n_tips
    gaps <- n_tips - 1
  } else if (direction == "semicircle") {
    size <- pi
    gaps <- n_tips - 1
  } else { # should not get here except for development failure
    stop("Unimplemented direction")
  }

  list(size=size, gaps=gaps, gap_size=size / gaps)
}

tree_xy <- function(s, t, direction) {
  if (direction %in% c("circle", "semicircle")) {
    x <- polar_x(t, s)
    y <- polar_y(t, s)
  } else if (direction %in% c("left", "right")) {
    x <- t
    y <- s
  } else if (direction %in% c("up", "down")) {
    x <- s
    y <- t
  } else {
    stop("Invalid direction")
  }
  list(x=x, y=y)
}

## Only use this within a drawDetails method, or all bets are off on a
## device resize.  I think that this is only an issue for the circular
## trees though.  Problem cases are polar_x and polar_y which go
## through convertWidth/convertHeight.
tree_location_resolve <- function(object, rotate_to_time=TRUE) {
  # Here, object is a list that must contain these keys:
  keys <- c("s", "t", "direction")
  if (!all(keys %in% names(object))) {
    stop("Missing keys: ",
         paste(setdiff(keys, names(object)), collapse=", "))
  }

  xy <- tree_xy(object$s, object$t, object$direction)

  if (object$direction %in% c("circle", "semicircle")) {
    if (rotate_to_time) {
      rot <- object$rot + to_degrees(object$s)
      rot <- rot %% 360
      i <- rot > 90 & rot < 270
      rot[i] <- (rot[i] + 180) %% 360
      hjust <- rep_len(0, length(rot))
      hjust[i] <- 1
      vjust <- 0.5
    } else {
      # TODO: This case here is totally not worked out yet.  Basically
      # we are going to have to slide the adjustment point around the
      # circle or around the bounding box.  It won't actually be hard,
      # but it will require changing both hjust and vjust
      rot <- 0
      hjust <- stop("Not yet implemented")
      vjust <- stop("Not yet implemented")
    }
  } else {
    if (object$direction %in% c("left", "right")) {
      rot <- object$rot
    } else {
      # TODO: Should this depend on rotate_to_time?
      rot <- object$rot + 90
    }
    hjust <- if (object$direction %in% c("left", "down")) 1 else 0
    vjust <- 0.5
  }

  list(x=xy$x, y=xy$y, hjust=hjust, vjust=vjust, rot=rot)
}

tree_segments_time <- function(s, t0, t1, direction, ...) {
  if (direction %in% c("circle", "semicircle")) {
    # TODO: Should t* be convered to native() here?
    grid.ray(t0, t1, s, ...)
  } else if (direction %in% c("left", "right")) {
    # TODO: Should we be using default.units="native" here?  Or t->native?
    grid.segments(t0, s, t1, s, ...)
  } else if (direction %in% c("up", "down")) {
    grid.segments(s, t0, s, t1, ...)
  }
}

tree_segments_spacing <- function(s0, s1, t, direction, ...) {
  if (direction %in% c("circle", "semicircle")) {
    # TODO: Should t* be convered to native() here?
    grid.arc(t, s0, s1, ...)
  } else if (direction %in% c("left", "right")) {
    grid.segments(t, s0, t, s1, ...)
  } else if (direction %in% c("up", "down")) {
    grid.segments(s0, t, s1, t, ...)
  }
}
