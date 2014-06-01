##' @export
##' @export forest_tree
forest_tree <-
  setRefClass("forest_tree",
              fields=list(
                ptr="externalptr"))

## TODO: It might be nicer to have the subtree and tree classes more
## tightly linked than the manual duplication here.  Consider either a
## flag on creation that indicates if it's a tree or subtree (but then
## both have same *actual* class), inheritance (but then we rewrite
## all methods), or something else.

## NOTE: Consider active binding functions here:
## labels=function(v) {
##   if (missing(v)) {
##     forest_tree__tip_labels(ptr)
##   } else {
##     field_readonly("labels")
##   }
## }
##
## field_readonly <- function(name) {
##   stop(sprintf("Field %s is read-only", name))
## }

# forest_tree$lock(names(forest_tree$fields()))
forest_tree$lock("ptr")

forest_tree$methods(initialize=function(x=NULL) {
  if (is.null(x)) {
    ptr <<- forest_tree__ctor_empty()$ptr
  } else if (inherits(x, "forest_node")) {
    ptr <<- forest_tree__ctor_node(x)$ptr
  } else if (inherits(x, "externalptr")) {
    ## Don't use this externally!
    ##
    ## TODO: Check the "type" attribute exists and is valid and that
    ## we're an externalptr here.
    assert_inherits(ptr, "externalptr")
    ptr <<- x
  } else if (inherits(x, "phylo")) {
    ptr <<- forest_from_ape(x)$ptr
  } else {
    stop("Can't construct a forest_tree from that, sorry :-/")
  }
})

## These might be better not as methods but as functions bound to the
## ptr, or to .self$ptr.  Check by benchmarking direct access and
## field access.  We can bind with all sorts of things, so this will
## end up being fairly transparent to the user.

## This block can easily be done in cog.  The issue will be whether we
## want to inject some documentation here at the same time.
##
## More unclear is which of these belong in the class (which ideally
## is fairly simple) and which belong as free functions.  That is
## something that there is definitely some continued tension about.
##
## One issue there is that we'll need some mechanism of dispatch for
## tree/subtree (so that's probably going to be S3), and we run the
## risk of being very confusing with ape functions.
##
## Of the functions in here, associate_data is the one that most
## clearly seems a candidate for removal.  check_names too.  One
## reason is that these functions are not core to "being" a tree, and
## because they have nontrivial arguments that would benefit from
## actual documentation.
forest_tree$methods(
  copy               = function() forest_tree__copy(ptr),
  is_empty           = function() forest_tree__empty(ptr),
  size               = function() forest_tree__size(ptr),
  arity              = function() forest_tree__arity(ptr),
  childless          = function() forest_tree__childless(ptr),
  representation     = function() forest_tree__representation(ptr),

  ## TODO: I'm back and forth on whether these should be
  ## count_tips/count_nodes or tips/nodes.
  count_tips         = function() forest_tree__count_tips(ptr),
  count_nodes        = function() forest_tree__count_nodes(ptr),
  tip_labels         = function() forest_tree__tip_labels(ptr),
  node_labels        = function() forest_tree__node_labels(ptr),
  heights            = function() forest_tree__heights(ptr),
  depths             = function() forest_tree__depths(ptr),
  is_binary          = function() forest_tree__is_binary(ptr),
  has_branch_lengths = function() forest_tree__has_branch_lengths(ptr),
  update_heights     = function() forest_tree__update_heights(ptr),
  collapse_singles   = function() forest_tree__collapse_singles(ptr),
  ladderise          = function() forest_tree__ladderise(ptr),
  copy_structure     = function() forest_tree__copy_structure(ptr),
  ## TODO: have a 'detatch' function, like in-place-copy

  ## And now we start getting into the functions that take arguments.
  ## Keeping these in sync is going to be annoying; could be worth
  ## moving to proper code generation for this stuff, but can always
  ## retrofit it.
  ##
  drop_tips      = function(labels)
  forest_tree__drop_tips_by_label(ptr, labels),
  rotate         = function(label)
  forest_tree__rotate(ptr, label),
  is_ultrametric = function(eps=sqrt(.Machine$double.eps))
  forest_tree__is_ultrametric(ptr, eps),
  check_names    = function(names, tip=TRUE, node=TRUE)
  forest_tree__check_names(ptr, names, tip, node),
  associate_data = function(data, tip, node)
  forest_tree__associate_data(ptr, data, tip, node)
)

## This is used to sanitise inputs where we always want a tree.  If a
## subtree is passed in, this will create a copy of the data.
tree_ptr <- function(tr) {
  if (inherits(tr, "forest_subtree")) {
    tr$to_tree()
  } else {
    tr
  }
}

## Subset methods:
forest_tree$methods(
  ## 0. root_subtree
  root_subtree = function() forest_tree__root_subtree(ptr),
  set_root_subtree = function(value)
  forest_tree__set_root_subtree(ptr, tree_ptr(value)),

  ## 1. root_node
  root_node = function() forest_tree__root_node(ptr),
  set_root_node = function(value)
  forest_tree__set_root_node(ptr, value),

  ## 2. child_subtree
  child_subtree     = function(idx) forest_tree__child_subtree(ptr, idx),
  set_child_subtree = function(idx, value)
  forest_tree__set_child_subtree(ptr, idx, tree_ptr(value)),

  ## 3. child_node
  child_node        = function(idx) forest_tree__child_node(ptr, idx),
  set_child_node    = function(idx, value)
  forest_tree__set_child_node(ptr, idx, value),

  ## 4. subtree
  subtree           = function(label) forest_tree__subtree(ptr, label),
  ## set_subtree    = function(label, value)
  ## forest_tree__set_subtree(ptr, label, value),

  ## Convenience functions:
  child             = function(idx, subtree=TRUE) {
    if (subtree) child_subtree(idx) else child_node(idx)
  },
  set_child         = function(idx, value) {
    if (inherits(value, "forest_subtree")) {
      set_child_subtree(idx, value)
    } else {
      set_child_node(idx, value)
    }
  }
  )

##' @export
##' @export forest_subtree
forest_subtree <- setRefClass("forest_subtree",
                              fields=list(ptr="externalptr"))
forest_subtree$lock(names(forest_subtree$fields()))

forest_subtree$methods(initialize=function(x=NULL) {
  if (inherits(x, "externalptr")) {
    ## Don't use this externally!
    ##
    ## TODO: Check the "type" attribute exists and is valid and that
    ## we're an externalptr here.
    assert_inherits(ptr, "externalptr")
    ptr <<- x
  } else {
    stop("Can't construct a forest_subtree from that, sorry :-/")
  }
})


forest_subtree$methods(
  # copy               = function() forest_subtree__copy(ptr),
  to_tree            = function() forest_subtree__to_tree(ptr),
  is_empty           = function() forest_subtree__empty(ptr),
  size               = function() forest_subtree__size(ptr),
  arity              = function() forest_subtree__arity(ptr),
  childless          = function() forest_subtree__childless(ptr),
  representation     = function() forest_subtree__representation(ptr),

  ## TODO: I'm back and forth on whether these should be
  ## count_tips/count_nodes or tips/nodes.
  count_tips         = function() forest_subtree__count_tips(ptr),
  count_nodes        = function() forest_subtree__count_nodes(ptr),
  tip_labels         = function() forest_subtree__tip_labels(ptr),
  node_labels        = function() forest_subtree__node_labels(ptr)
  # heights            = function() forest_subtree__heights(ptr),
  # depths             = function() forest_subtree__depths(ptr),
  # is_binary          = function() forest_subtree__is_binary(ptr),
  # has_branch_lengths = function() forest_subtree__has_branch_lengths(ptr),
  # update_heights     = function() forest_subtree__update_heights(ptr),
  # collapse_singles   = function() forest_subtree__collapse_singles(ptr),
  # ladderise          = function() forest_subtree__ladderise(ptr),
  # copy_structure     = function() forest_subtree__copy_structure(ptr),
  ## TODO: have a 'detatch' function, like in-place-copy
  )

## Subset methods:
forest_subtree$methods(
  ## 0. root_subtree
  root_subtree = function() forest_subtree__root_subtree(ptr),
  set_root_subtree = function(value)
  forest_subtree__set_root_subtree(ptr, tree_ptr(value)),

  ## 1. root_node
  root_node = function() forest_subtree__root_node(ptr),
  set_root_node = function(value)
  forest_subtree__set_root_node(ptr, value),

  ## 2. child_subtree
  child_subtree     = function(idx) forest_subtree__child_subtree(ptr, idx),
  set_child_subtree = function(idx, value)
  forest_subtree__set_child_subtree(ptr, idx, tree_ptr(value)),

  ## 3. child_node
  child_node        = function(idx) forest_subtree__child_node(ptr, idx),
  set_child_node    = function(idx, value)
  forest_subtree__set_child_node(ptr, idx, value),

  ## 4. subtree
  subtree           = function(label) forest_subtree__subtree(ptr, label),
  ## set_subtree    = function(label, value)
  ## forest_subtree__set_subtree(ptr, label, tree_ptr(value)),

  ## Convenience functions:
  child             = function(idx, subtree=TRUE) {
    if (subtree) child_subtree(idx) else child_node(idx)
  },
  set_child         = function(idx, value) {
    if (inherits(value, "forest_subtree")) {
      set_child_subtree(idx, value)
    } else {
      set_child_node(idx, value)
    }
  }
  )
