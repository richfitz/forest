## Add things to a tree
##
## The ideas in this section borrow *very* heavily from ggplot2.

##' @S3method + tree
`+.tree` <- function(e1, e2) {
  add_to_tree(e2, e1) # e1 is the tree!
}

## Note that the tree_grob is the *second* argument, because dispatch
## needs to happen on the class of object being added.  There is
## probably nice mix of partial application and generic programming
## that could be used here, but it really doesn't matter at this
## point: this is all non-api.
add_to_tree <- function(object, tree_grob, ...) {
  UseMethod("add_to_tree")
}

add_to_tree.tree_labels <- function(object, tree_grob, ...) {
  branches <- tree_grob$children$branches
  i <- ( branches$is_tip & object$tip) |
       (!branches$is_tip & object$node)
  label <- branches$label[i]
  at <- tree_offset(tree_label_coords(label, tree_grob),
                    object$offset, tree_grob$direction)
  lab <- tree_labelGrob(label, at$t, at$s,
                        direction=tree_grob$direction, rot=object$rot,
                        name=object$name, gp=object$gp,
                        vp=tree_grob$childrenvp)
  addGrob(tree_grob, lab)
}

add_to_tree.tree_style <- function(object, tree_grob, ...) {
  targets <- object$targets
  base <- object$base
  what <- object$what
  cl <- classify(tree_grob$tree, names(targets)) + 1L

  for (w in what) {
    if (!(w %in% names(tree_grob$children)))
      stop("No child member ", w, " within this tree")
    thing <- tree_grob$children[[w]]
    if (!("label" %in% names(thing)))
      stop("Tree member ", w, " does not have a label member")
    base.w <- if (is.null(base)) thing$gp else base
    # TODO: It might be best here to delay this till the drawDetails
    # part, or at least retrigger base lookup.  That would not be hard
    # to do, and actually allow restyling a bit more easily
    # potentially.
    #
    # TODO: perhaps first look to the object's gpar?  Or would
    # deferring to drawing solve this too?
    tree_grob$children[[w]]$gp <-
      combine_gpars(c(list(base.w), targets), unname(cl[thing$label]))
  }

  # TODO: Here, and in add_to_tree.tree_labels, or in add_to_tree, or
  # in `+.tree`, should the return be invisible?
  tree_grob
}

# TODO: At the moment, this is set up only for a single tip and not
# for arbitrary plotting.  Lotsa changes coming.
add_to_tree.tree_image <- function(object, tree_grob, ...) {
  at <- tree_offset(tree_label_coords(object$label, tree_grob),
                    object$offset, tree_grob$direction)
  img <- tree_imageGrob(object$image, at$t, at$s,
                        direction=tree_grob$direction,
                        size=object$size, rot=object$rot,
                        name=object$name, gp=object$gp,
                        vp=tree_grob$childrenvp)
  addGrob(tree_grob, img)
}

add_to_tree.tree_brace <- function(object, tree_grob, ...) {
  # Fraction of the gap we can use.  Hard coded for now.  Needs to be
  # on [0,1]
  p_gap <- 2/3

  # Extend into the gap by this amount:
  ds <- p_gap * tree_grob$spacing_info$gap_size / 2
  # (the /2 is because the gap is shared with the neighbouring tips).

  brace_position <- function(label) {
    desc <- tree_grob$tree$get_subtree(label)$tip_labels
    branches <- tree_grob$children$branches
    i <- match(desc, branches$label)
    # Here, even though min and max are the same for tips at the moment
    # (and the same as mid) I'm using the min and max values
    # separately.  This might be useful later for clade trees.
    s_min <- min(branches$spacing_min[i]) - ds
    s_max <- max(branches$spacing_max[i]) + ds
    t <- max(branches$time_tip[i])
    c(s_min=s_min, s_max=s_max, t=t)
  }

  at <- sapply(object$label, brace_position)

  # Resolve the brace alignment:
  if (object$alignment == "set") {
    at["t",] <- max(at["t",])
  } else if (object$alignment == "global") {
    at["t",] <- max(tree_grob$children$branches$time_tip)
  }

  at <- list(s_min=at["s_min",], s_max=at["s_max",], t=at["t",])

  # Ignoring the possibility of placing outside of labels for now, but
  # allowing for a user-specified offset:
  at <- tree_offset(at, object$offset, tree_grob$direction)

  brace <- tree_braceGrob(object$label, at$t, at$s_min, at$s_max,
                          direction=tree_grob$direction,
                          name=object$name, gp=object$gp,
                          vp=tree_grob$childrenvp)
  addGrob(tree_grob, brace)
}

tree_label_coords <- function(label, tree_grob) {
  if (any(is.na(label))) {
    stop("label cannnot be missing")
  }
  branches <- tree_grob$children$branches
  i <- match(label, branches$label)
  if (any(is.na(i))) {
    stop(sprintf("labels %s not found in the tree",
                 paste(label[is.na(i)])))
  }
  # TODO: class this, and then depend on that class elsewhere?  This
  # could then be the generic location object.
  #
  # TODO: Do we also need a label here for future lookup?  Could be
  # too much?
  #
  # NOTE: Neither s nor t are units here, but are intrepreted as
  # native units on a scale that is not directly related to the
  # device.
  list(s=branches$spacing_mid[i],
       t=branches$time_tip[i])
}

tree_offset <- function(at, offset_t, direction) {
  if (!is.unit(offset_t)) {
    stop("offset_t must be a unit")
  }
  # NOTE: In contrast with tree_label_coords, this converts the 't'
  # member into a native unit.  The 's' member stays as a non-unit
  # though.  This is probably undesirable, and it might be better to
  # convert to a native unit in tree_label_coords directly.
  at$t <- native(at$t) + normalise_time(offset_t, direction)
  at
}
