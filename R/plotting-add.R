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
  direction <- tree_grob$direction
  offset_t <- normalise_time(object$offset, direction)
  branches <- tree_grob$children$branches
  i <- ( branches$is_tip & object$tip) |
       (!branches$is_tip & object$node)

  label <- branches$label[i]
  t <- native(branches$time_tipward[i]) + offset_t
  s <- branches$spacing_mid[i]

  lab <- tree_labelGrob(label, t, s, direction, object$rot,
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
  direction <- tree_grob$direction
  at <- tree_label_coords(object$label, tree_grob)
  offset_t <- normalise_time(object$offset, direction)
  at$t <- native(at$t) + offset_t
  img <- tree_imageGrob(object$image, at$t, at$s, direction=direction,
                        size=object$size, rot=object$rot,
                        name=object$name, gp=object$gp,
                        vp=tree_grob$childrenvp)
  addGrob(tree_grob, img)
}

tree_label_coords <- function(label, tree_grob) {
  branches <- tree_grob$children$branches
  i <- match(label, branches$label)
  if (any(is.na(i))) {
    stop(sprintf("labels %s not found in the tree",
                 paste(label[is.na(i)])))
  }
  list(s=branches$spacing_mid[i],
       t=branches$time_tip[i])
}
