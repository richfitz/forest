## Add things to a tree
##
## The ideas in this section borrow *very* heavily from ggplot2.

##' @export
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
  lab <- tree_labelsGrob(label, at$t, at$s,
                         direction=tree_grob$direction, rot=object$rot,
                         name=object$name, gp=object$gp,
                         vp=tree_grob$childrenvp)
  addGrob(tree_grob, lab)
}

add_to_tree.tree_style <- function(object, tree_grob, ...) {
  # TODO: Question here is what to do if a target is not found?
  # Probably that's a job for tree_match though, and have an argument
  # must_match or something?
  class   <- object$class
  name    <- object$name
  paths <- tree_match(tree_grob, class, name)

  targets <- object$targets
  base    <- object$base

  if (!object$descendants) {
    # This would normally be done in classify(), but we'll sanitise
    # input the same way:
    #
    # TODO: Possibly merge this into classify, or wrap it up a bit for
    # use here.
    if (any(duplicated(targets))) {
      stop("Labels must be unique")
    }
    cl <- classify(tree_grob$tree, character(0)) + 1L
    cl[names(targets)] <- seq_along(targets) + 1L
  } else {
    cl <- classify(tree_grob$tree, names(targets)) + 1L
  }

  for (p in paths) {
    thing <- getGrob(tree_grob, p)
    if (!("label" %in% names(thing)))
      stop("Tree member ", dQuote(p), " does not have a label member")
    base.p <- if (is.null(base)) thing$gp else base
    # TODO: It might be best here to delay this till the drawDetails
    # part, or at least retrigger base lookup.  That would not be hard
    # to do, and actually allow restyling a bit more easily
    # potentially.
    #
    # TODO: perhaps first look to the object's gpar?  Or would
    # deferring to drawing solve this too?
    thing$gp <-
      combine_gpars(c(list(base.p), targets), unname(cl[thing$label]))
    tree_grob <- setGrob(tree_grob, p, thing)
  }

  # TODO: Here, and in add_to_tree.tree_labels, or in add_to_tree, or
  # in `+.tree`, should the return be invisible?
  tree_grob
}

add_to_tree.tree_braces <- function(object, tree_grob, ...) {
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

  brace <- tree_bracesGrob(object$label, at$t, at$s_min, at$s_max,
                           direction=tree_grob$direction,
                           name=object$name, gp=object$gp,
                           vp=tree_grob$childrenvp)
  addGrob(tree_grob, brace)
}

add_to_tree.tree_objects <- function(object, tree_grob, ...) {
  label <- names(object$objects)
  at <- tree_offset(tree_label_coords(label, tree_grob),
                    object$offset, tree_grob$direction)
  gr <- tree_objectsGrob(label=label, object$objects, at$t, at$s,
                         direction=tree_grob$direction,
                         width=object$width,
                         rot=object$rot, class=class(object),
                         name=object$name,
                         gp=object$gp, vp=tree_grob$childrenvp)
  addGrob(tree_grob, gr)
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
