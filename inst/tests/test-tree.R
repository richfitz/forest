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

  heights.cmp <- sort(diversitree:::branching.heights(phy))
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

## TODO: This needs more extensive testing:
##   * more than one singleton
##   * comparison with ape, especially handling of node labels
##   * edge lengths
test_that("collapse_singles", {
  source("helper-forest.R")
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
