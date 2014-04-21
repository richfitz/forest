## Functions that define the grobs used in plotting.  Thes are all
## really really simple and at most do some basic checking of input
## types before passing everything into `grob` to do the assembly
## (which is basically just putting everything into a list with a
## particular class vector).

## Tree branches (not stored as separate time and spacing grobs
## because the details of how those are drawn may change).
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

## Labels
##
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
tree_labelsGrob <- function(label, t, s, direction, rot=0,
                            name=NULL, gp=gpar(), vp=NULL) {
  if (!is.numeric(s))
    stop("s must be numeric")
  if (!is.unit(t))
    stop("t must be a unit")
  grob(label=label, t=t, s=s, direction=direction, rot=rot,
       name=name, gp=gp, vp=vp, cl="tree_labels")
}

tree_bracesGrob <- function(label, t, s_min, s_max, direction,
                            name=NULL, gp=gpar(), vp=NULL) {
  if (!is.numeric(s_min) || !is.numeric(s_max))
    stop("s_min and s_max must be numeric")
  if (!is.unit(t))
    stop("t must be a unit")
  grob(label=label, t=t, s_min=s_min, s_max=s_max, direction=direction,
       name=name, gp=gp, vp=vp, cl="tree_braces")
}

## Generic object used for images at the moment, but will be useful
## for things like mini-plots.  Not yet vectorised on draw, but that's
## not asserted here.
tree_objectsGrob <- function(label, objects, t, s, direction, width,
                             rot=0, name=name, gp=gpar(), vp=NULL) {
  assert_number(s)
  assert_unit(t)
  assert_unit(width)
  grob(label=label, objects=objects, t=t, s=s, direction=direction,
       width=width, rot=rot,
       name=name, gp=gp, vp=vp, cl="tree_objects")
}
