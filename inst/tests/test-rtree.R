source("helper-forest.R")

# Most of the things here will be tested more thoroughly in the main
# tree testing (test-tree and test-itree).  This file will change
# considerably during the reorganisation).

tree_of <- make.tree_of(rtree)

context("Simple tree")

test_that("Empty tree", {
  tr <- new(rtree)
  expect_that(tr$empty,          is_true())
  expect_that(tr$size,           equals(0))
  expect_that(tr$arity,          equals(0))
  expect_that(tr$childless,      is_true())
  expect_that(tr$representation, equals(""))

  expect_that(tr$tips,           equals(0))
  expect_that(tr$nodes,          equals(0))
})

test_that("Construct with root node", {
  nd <- new(rnode, "root", pi, list(1, 2))
  tr <- new(rtree, nd)
  expect_that(tr$empty,          is_false())
  expect_that(tr$size,           equals(1))
  expect_that(tr$arity,          equals(0))
  expect_that(tr$childless,      is_true())
  expect_that(tr$representation, equals("root"))

  expect_that(tr$tips,           equals(1))
  expect_that(tr$nodes,          equals(0)) # NOTE: sensible?

  nd2 <- tr$root_node
  expect_that(nd2$equals(nd), is_true())
  nd2$length <- 0
  # Does not flow through at all:
  expect_that(nd$length, equals(pi))
  expect_that(tr$root_node$length, equals(pi))
  expect_that(tr$tip_labels, equals(nd$label))

  expect_that(tr$is_binary, throws_error()) # not defined

  tr2 <- tr$copy_structure()
  expect_that(tr2$root_node$label,  is_identical_to(tr$root_node$label))
  expect_that(tr2$root_node$length, is_identical_to(tr$root_node$length))
  expect_that(tr2$root_node$data,   equals(NULL))
})

## A couple of little ape trees for testing
set.seed(1)
phy <- ape::rtree(5)
phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

# Version with no edge lengths:
phy0 <- phy
phy0$edge.length <- NULL

tr <- forest.from.ape(phy)
tr0 <- forest.from.ape(phy0)

test_that("tips and nodes works", {
  expect_that(tr$tips,        equals(Ntip(phy)))
  expect_that(tr$nodes,       equals(phy$Nnode))
  expect_that(tr$tip_labels,  equals(phy$tip.label))
  expect_that(tr$node_labels, equals(phy$node.label))

  expect_that(tr[[1]]$tips,        equals(2))
  expect_that(tr[[1]]$nodes,       equals(1))
  expect_that(tr[[1]]$tip_labels,  equals(c("t3", "t4")))
  expect_that(tr[[1]]$node_labels, equals("n7"))
})

test_that("is_binary", {
  tr0 <- new(rtree)
  tr1 <- new(rtree, new(rnode, "a"))
  tr2 <- from.newick.string("(b,c)a;")
  tr3 <- from.newick.string("(b,c,d)a;")
  tr4 <- from.newick.string("((1,2)7,(3,(4,5)9)8)6;")
  tr5 <- from.newick.string("((1,2,10)7,(3,(4,5)9)8)6;")

  expect_that(tr0$is_binary, throws_error())
  expect_that(tr1$is_binary, throws_error())
  expect_that(tr2$is_binary, is_true())
  expect_that(tr3$is_binary, is_false())
  expect_that(tr4$is_binary, is_true())
  expect_that(tr5$is_binary, is_false())
})

test_that("has_branch_lengths", {
  expect_that(tr$has_branch_lengths,  is_true())
  expect_that(tr0$has_branch_lengths, is_false())
})

test_that("Height calculation", {
  set.seed(1)
  phy <- ape::rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  tr$update_heights()
  heights <- unlist(treeapply(tr, function(nd)
                              structure(nd$height, names=nd$label)))
  depths <- unlist(treeapply(tr, function(nd)
                              structure(nd$depth, names=nd$label)))

  if (suppressWarnings(require("diversitree", quietly=TRUE)))
    branching.heights <- diversitree:::branching.heights
  else
    branching.heights <- dt_branching.heights

  heights.cmp <- sort(branching.heights(phy))
  depths.cmp <- max(heights.cmp) - heights.cmp

  expect_that(heights[names(heights.cmp)], equals(heights.cmp))
  expect_that(depths[names(depths.cmp)],   equals(depths.cmp))

  expect_that(tr$heights, is_identical_to(unname(heights)))
  expect_that(tr$depths,  is_identical_to(unname(depths)))

  ## With no edge lengths, we will refuse to do this.
  phy[["edge.length"]] <- NULL
  tr <- from.newick.string(write.tree(phy))
  expect_that(tr$update_heights(), throws_error())
})

test_that("get_subtree", {
  set.seed(1)
  phy <- ape::rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)
  str <- tr$representation

  expect_that(tr$get_subtree("not_in_tree"), throws_error())
  expect_that(tr$get_subtree("t1"),          throws_error())

  target <- "n2"
  sub2 <- tr$get_subtree(target)

  ## Modify the subtree:
  nd <- sub2$root_node
  nd$label <- "subtree_root"
  sub2$root_node <- nd

  ## Test that propogates to the main tree:
  str2 <- sub(target, nd$label, str)
  expect_that(tr$representation, is_identical_to(str2))

  expect_that(sub2$get_subtree("not_in_tree"), throws_error())
  expect_that(sub2$get_subtree("t6"),          throws_error())
  # Node exists, but not within this subtree, so error:
  expect_that(sub2$get_subtree("n8"),          throws_error())

  ## Can further get a subtree of a subtree:
  target2 <- "n3"
  sub3 <- sub2$get_subtree(target2)

  ## Modify the subtree:
  nd3 <- sub3$root_node
  nd3$label <- "subsubtree_root"
  sub3$root_node = nd3

  ## Test that propogates to the main tree:
  str3 <- sub(target2, nd3$label, str2)
  expect_that(tr$representation, is_identical_to(str3))

  ## Extract a further subtree from the main tree, but convert to full
  ## tree:
  sub4 <- tr$get_subtree("n4")$to_tree()
  nd4 <- sub4$root_node
  nd4$label <- "newtree_root"
  sub4$root_node = nd4
  ## This change has *not* propagated up to the main tree:
  expect_that(tr$representation, is_identical_to(str3))
})

test_that("is_tree_ultrametric", {
  set.seed(1)
  phy <- ape::rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  tol <- .Machine$double.eps^0.5
  expect_that(tr$is_ultrametric(tol), is_false())

  set.seed(1)
  phy <- rcoal(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  expect_that(tr$is_ultrametric(tol), is_true())

  sub <- tr$get_subtree("n8")
  nd <- sub$root_node
  nd$length <- nd$length + 2*tol
  sub$root_node <- nd
  expect_that(tr$is_ultrametric(tol), is_false())

  ## And try on a tree with no branch lengths:
  phy[["edge.length"]] <- NULL
  tr <- forest.from.ape(phy)
  expect_that(tr$is_ultrametric(tol), throws_error())
})


## TODO: This needs more extensive testing:
##   * more than one singleton
##   * comparison with ape, especially handling of node labels
##   * edge lengths
test_that("collapse_singles", {
  cmp <- from.newick.string("((1,2)7,3)6;")
  cmp.cpy <- cmp$copy()
  cmp$collapse_singles()
  expect_that(cmp$equals(cmp.cpy), is_true())

  ## Then a tree with singles
  cmp <- from.newick.string("((1)7,3)6;")
  cmp.correct <- from.newick.string("(1,3)6;")

  cmp.cpy <- cmp$copy()
  cmp$collapse_singles()
  expect_that(cmp$equals(cmp.cpy),     is_false())
  expect_that(cmp$equals(cmp.correct), is_true())
})

## The notorious drop.tip()
test_that("drop_tip", {
  set.seed(1)
  phy <- ape::rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  expect_that(tr$drop_tip("not_in_tree"), throws_error())

  n.tips <- tr$tips
  n.nodes <- tr$nodes
  tip.labels <- tr$tip_labels
  node.labels <- tr$node_labels

  tr$drop_tips("t1")

  expect_that(tr$tips, equals(n.tips-1))
  expect_that(tr$tip_labels, equals(setdiff(tip.labels, "t1")))
  expect_that(tr$nodes, equals(n.nodes - 1))
  expect_that(tr$node_labels, equals(setdiff(node.labels, "n6")))
})

## Rotate a node:
test_that("rotate", {
  set.seed(1)
  phy <- ape::rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)

  expect_that(tr$rotate("not_in_tree"), throws_error())
  expect_that(tr$rotate("t1"),          throws_error())
  # TODO: To test for error: node of arity 1 and 3?

  # n9 subtends t4 and t8
  sub9 <- tr$get_subtree("n9")
  expect_that(sub9$representation,
              is_identical_to("n9(t8 t4)"))
  tr$rotate("n9")
  expect_that(sub9$representation,
              is_identical_to("n9(t4 t8)"))

  # n2 subtends three species:
  sub2 <- tr$get_subtree("n2")
  expect_that(sub2$representation,
              is_identical_to("n2(t10 n3(t6 t9))"))
  tr$rotate("n2")
  expect_that(sub2$representation,
              is_identical_to("n2(n3(t6 t9) t10)"))

  expect_that(sub2$rotate("not_in_tree"), throws_error())
  expect_that(sub2$rotate("t1"),          throws_error())
  expect_that(sub2$rotate("n8"),          throws_error())

  sub2$rotate("n3")
  expect_that(sub2$representation,
              is_identical_to("n2(n3(t9 t6) t10)"))
})

test_that("ladderise", {
  set.seed(1)
  phy <- ape::rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))

  ## Ladderize left and right:
  phy.l <- ladderize(phy, FALSE)
  phy.r <- ladderize(phy, TRUE)

  tr <- forest.from.ape(phy)

  tr.l <- tr$copy()
  tr.l$ladderise(FALSE)

  tr.r <- tr$copy()
  tr.r$ladderise(TRUE)

  ## TODO: Still issues with tree comparisons to deal with:
  expect_that(to.newick.string(tr.l),
              is_identical_to(write.tree(phy.l)))
  expect_that(to.newick.string(tr.r),
              is_identical_to(write.tree(phy.r)))
})
