source("helper-forest.R")

context("General tree")

tree_of <- make.tree_of(xtree)

set.seed(1)
phy <- rtree(5)
phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

# Version with no edge lengths:
phy0 <- phy
phy0$edge.length <- NULL

tree_of <- make.tree_of(xtree)
x <- make.node.builder.xnode(phy)
cmp <- tree_of(x(6))(tree_of(x(7))(x(1), x(2)),
                     tree_of(x(8))(x(3), tree_of(x(9))(x(4), x(5))))()

x <- make.node.builder.xnode(phy0)
cmp0 <- tree_of(x(6))(tree_of(x(7))(x(1), x(2)),
                      tree_of(x(8))(x(3), tree_of(x(9))(x(4), x(5))))()

test_that("tips and nodes works", {
  expect_that(cmp$tips,        equals(Ntip(phy)))
  expect_that(cmp$nodes,       equals(phy$Nnode))
  expect_that(cmp$tip_labels,  equals(phy$tip.label))
  expect_that(cmp$node_labels, equals(phy$node.label))

  expect_that(cmp[[1]]$tips,        equals(2))
  expect_that(cmp[[1]]$nodes,       equals(1))
  expect_that(cmp[[1]]$tip_labels,  equals(c("t3", "t4")))
  expect_that(cmp[[1]]$node_labels, equals("n7"))
})

test_that("is_binary", {
  nd <- function(...) new(xnode, ...)
  tr0 <- new(xtree)
  expect_that(tr0$is_binary, throws_error())

  tr1 <- tree_of(nd("a"))()
  expect_that(tr$is_binary, throws_error())

  tr2 <- tree_of(nd("a"))(nd("b"),nd("c"))()
  expect_that(tr2$is_binary, is_true())

  tr3 <- tree_of(nd("a"))(nd("b"),nd("c"),nd("d"))()
  expect_that(tr3$is_binary, is_false())

  tr4 <- tree_of(nd("6"))(tree_of(nd("7"))(nd("1"), nd("2")),
                       tree_of(nd("8"))(nd("3"),
                                        tree_of(nd("9"))(nd("4"), nd("5"))))()
  expect_that(tr4$is_binary, is_true())

  tr5 <- tree_of(nd("6"))(tree_of(nd("7"))(nd("1"), nd("2"), nd("10")),
                       tree_of(nd("8"))(nd("3"),
                                        tree_of(nd("9"))(nd("4"), nd("5"))))()
  expect_that(tr5$is_binary, is_false())
})

test_that("has_branch_lengths", {
  expect_that(cmp$has_branch_lengths,  is_true())
  expect_that(cmp0$has_branch_lengths, is_false())
})

test_that("Height calculation", {
  ## This is a hack to generate a reasonable length tree:
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- from.newick.string(write.tree(phy))

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

test_that("is_tree_ultrametric", {
  ## This is a hack to generate a reasonable length tree:
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- from.newick.string(write.tree(phy))
  tol <- .Machine$double.eps^0.5
  expect_that(tr$is_ultrametric(tol), is_false())

  set.seed(1)
  phy <- rcoal(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- from.newick.string(write.tree(phy))
  tol <- .Machine$double.eps^0.5
  expect_that(tr$is_ultrametric(tol), is_true())

  ## Move a branch just slightly:
  nd <- tr$begin_post()$value
  nd$length <- nd$length + 2*tol
  tr$begin_post()$assign(nd)
  expect_that(tr$is_ultrametric(tol), is_false())

  ## And try on a tree with no branch lengths:
  phy[["edge.length"]] <- NULL
  tr <- from.newick.string(write.tree(phy))
  expect_that(tr$is_ultrametric(tol), throws_error())
})

test_that("get_subtree", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- forest.from.ape(phy)
  str <- tr$representation

  expect_that(tr$get_subtree("not_in_tree"), throws_error())
  expect_that(tr$get_subtree("t1"),          throws_error())

  target <- "n2"
  sub2 <- tr$get_subtree(target)

  ## Modify the subtree:
  nd <- sub2$root()
  nd$label <- "subtree_root"
  sub2$begin()$assign(nd)

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
  nd3 <- sub3$root()
  nd3$label <- "subsubtree_root"
  sub3$begin()$assign(nd3)

  ## Test that propogates to the main tree:
  str3 <- sub(target2, nd3$label, str2)
  expect_that(tr$representation, is_identical_to(str3))

  ## Extract a further subtree from the main tree, but convert to full
  ## tree:
  sub4 <- tr$get_subtree("n4")$to_tree()
  nd4 <- sub4$root()
  nd4$label <- "newtree_root"
  sub4$begin()$assign(nd4)
  ## This change has *not* propagated up to the main tree:
  expect_that(tr$representation, is_identical_to(str3))
})

## TODO: This needs more extensive testing:
##   * more than one singleton
##   * comparison with ape, especially handling of node labels
##   * edge lengths
test_that("collapse_singles", {
  tree_of <- make.tree_of(xtree)

  nd <- function(x, ...) new(xnode, as.character(x), ...)

  ## First, tree with no singles
  cmp <- tree_of(nd(6))(tree_of(nd(7))(nd(1), nd(2)),
                       tree_of(nd(8))(nd(3), tree_of(nd(9))(nd(4), nd(5))))()

  cmp <- tree_of(nd(6))(tree_of(nd(7))(nd(1), nd(2)),
                        nd(3))()
  cmp.cpy <- cmp$copy()
  cmp$collapse_singles()
  expect_that(cmp$equals(cmp.cpy), is_true())

  ## Then a tree with singles; this does not work; it decided then
  ## that node 6 was single!  Right; so what happenned here is that we
  ## delete the entire sub tree '7(1)' leaving '6(3)'; 6 now has arity
  ## 1 so we delete that too.  Need to to something a bit more clever
  ## here!
  cmp <- tree_of(nd(6))(tree_of(nd(7))(nd(1)), nd(3))()
  cmp.correct <- tree_of(nd(6))(nd(1),nd(3))()

  cmp.cpy <- cmp$copy()
  cmp$collapse_singles()
  expect_that(cmp$equals(cmp.cpy),     is_false())
  expect_that(cmp$equals(cmp.correct), is_true())
})

## The notorious drop.tip()
test_that("drop_tip", {
  set.seed(1)
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- from.newick.string(write.tree(phy))

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
  phy <- rtree(10)
  phy$node.label <- paste0("n", seq_len(phy$Nnode))
  tr <- from.newick.string(write.tree(phy))

  expect_that(tr$rotate("not_in_tree"), throws_error())
  expect_that(tr$rotate("t1"),          throws_error())
  # TODO: To test for error: node of arity 1 and 3?

  # n9 subtends t4 and t8
  expect_that(tr[[2]][[2]][[1]]$representation,
              is_identical_to("n9(t8 t4)"))
  tr$rotate("n9")
  expect_that(tr[[2]][[2]][[1]]$representation,
              is_identical_to("n9(t4 t8)"))

  # n2 subtends
  expect_that(tr[[1]]$representation,
              is_identical_to("n2(t10 n3(t6 t9))"))
  tr$rotate("n2")
  expect_that(tr[[1]]$representation,
              is_identical_to("n2(n3(t6 t9) t10)"))
})
