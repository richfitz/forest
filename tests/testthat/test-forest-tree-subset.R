source("helper-forest.R")

## TODO: Function that gives child node names
## TODO: Function that gives vector of child nodes
## TODO: Function that gives vector of child subtrees
## TODO: Function, like subtree(), that gets the node
## TODO: Properly consider active binding functions

context("forest_tree (subsetting)")

## TODO: Once 'equals' is worked out, get that used here (ideally, get
## '==' implemented but not sure how to do that for reference
## classes).

## A couple of little ape trees for testing
set.seed(1)
phy <- ape::rtree(5)
phy$node.label <- paste0("n", seq_len(phy$Nnode) + Ntip(phy))

phy2 <- ape::rtree(3)
phy2$node.label <- paste0("n", seq_len(phy2$Nnode) + Ntip(phy2))

# Version with no edge lengths:
phy0 <- phy
phy0$edge.length <- NULL

test_that("root_subtree", {
  tr <- forest_tree(phy)
  tr2 <- forest_tree(phy2)

  str <- tr$root_subtree()
  expect_that(str, is_a("forest_subtree"))
  expect_that(str$size(),           equals(tr$size()))
  expect_that(str$representation(), equals(tr$representation()))
  ## Elephants all the way down:
  sstr <- str$root_subtree()
  expect_that(sstr$representation(), equals(str$representation()))

  tr_same <- tr
  tr_copy <- tr$copy()

  cmp  <- tr$representation()
  cmp2 <- tr2$representation()

  ## First, set tree to be itself - changing nothing.
  tr$set_root_subtree(tr)
  expect_that(tr$representation(), equals(cmp))
  expect_that(tr_same$representation(), equals(cmp))
  expect_that(tr_copy$representation(), equals(cmp))

  ## Set root subtree to be something else:
  tr$set_root_subtree(tr2)
  expect_that(tr$representation(),      equals(cmp2))
  expect_that(tr_same$representation(), equals(cmp2))
  expect_that(tr_copy$representation(), equals(cmp))

  ## Set root subtree to be a tree generated from the original root
  ## subtree (phew!)
  tr$set_root_subtree(tr_copy$root_subtree())
  expect_that(tr$representation(), equals(cmp))
  expect_that(tr_same$representation(), equals(cmp))
  expect_that(tr_copy$representation(), equals(cmp))
})

test_that("root_node", {
  tr0 <- forest_tree()
  expect_that(tr0$root_node(), throws_error("empty tree"))
  ## Options here for set: error or append (TODO)

  nd <- forest_node("root", pi, list(1, 2))
  tr1 <- forest_tree(nd)
  str1 <- tr1$root_subtree()

  expect_that(tr1$root_node(), is_identical_to(nd))
  expect_that(str1$root_node(), is_identical_to(nd))

  ## Setting the root node changes it in both the tree and the
  ## subtree.
  nd2 <- forest_node("root2", exp(1), letters[1:2])
  tr1$set_root_node(nd2)
  expect_that(tr1$root_node(),  is_identical_to(nd2))
  expect_that(str1$root_node(), is_identical_to(nd2))

  ## Can't set a subtree.
  expect_that(tr1$set_root_node(str1),
              throws_error("Expected forest_node, recieved forest_subtree"))
})

test_that("child_subtree", {
  tr <- forest_tree(phy)

  s1 <- tr$child_subtree(1)
  s2 <- tr$child_subtree(2)
  expect_that(s1, is_a("forest_subtree"))
  expect_that(s2, is_a("forest_subtree"))

  ## This should be dynamically computed, but that's actually hard to
  ## do in ape.
  expect_that(s1$size(), equals(3))
  expect_that(s2$size(), equals(5))
  expect_that(s1$size() + s2$size(), equals(tr$size() - 1))

  ## We have successfully pulled apart the tree.
  expect_that(sprintf("%s(%s %s)",
                      tr$root_node()$label,
                      s1$representation(),
                      s2$representation()),
              equals(tr$representation()))

  ## Default is to get subtree
  c1 <- tr$child(1)
  expect_that(c1, is_a("forest_subtree"))
  expect_that(c1$representation, equals(s1$representation))

  ## Explicit argument is TRUE for subtree
  c1 <- tr$child(1, TRUE)
  expect_that(c1, is_a("forest_subtree"))
  expect_that(c1$representation, equals(s1$representation))

  ## Bounds checked:
  expect_that(tr$child_subtree(-1), # this is the index checking
              throws_error("Required a non-negative value"))
  ## This is the actual bounds checking
  expect_that(tr$child_subtree(0),  throws_error("out of bounds"))
  expect_that(tr$child_subtree(3),  throws_error("out of bounds"))

  ## Assignment.
  ## Also confirm how this will behave for:
  str  <- tr$root_subtree()   # the subtree comprising the full tree.
  str1 <- tr$child_subtree(1) # the subtree that is being affected
  tr2  <- tr$copy()           # an independent copy of the tree
  str2 <- str1$to_tree()      # an independent copy of the child subtree

  ## Representations of the affected tree and subtree
  rep_tr  <- tr$representation()
  rep_str1 <- str1$representation()



})

test_that("child_node", {
  tr <- forest_tree(phy)
  other <- forest_tree(phy2)

  nd1 <- tr$child_node(1)
  nd2 <- tr$child_node(2)
  expect_that(nd1, is_a("forest_node"))
  expect_that(nd2, is_a("forest_node"))

  expect_that(nd1, equals(tr$child_subtree(1)$root_node()))
  expect_that(nd2, equals(tr$child_subtree(2)$root_node()))

  expect_that(tr$child(1, FALSE), is_identical_to(nd1))

  ## Bounds checked:
  expect_that(tr$child_node(-1), # this is the index checking
              throws_error("Required a non-negative value"))
  ## This is the actual bounds checking
  expect_that(tr$child_node(0),  throws_error("out of bounds"))
  expect_that(tr$child_node(3),  throws_error("out of bounds"))

  ## Assignment.
  ## Also confirm how this will behave for:
  str  <- tr$root_subtree()   # the subtree comprising the full tree.
  str1 <- tr$child_subtree(1) # the subtree that is being affected
  tr2  <- tr$copy()           # an independent copy of the tree
  str2 <- str1$to_tree()      # an independent copy of the child subtree

  ## Representations of the affected tree and subtree
  rep_tr  <- tr$representation()
  rep_str1 <- str1$representation()

  tr$set_child_subtree(1, other)

  ## Did not change the independent copies:
  expect_that(tr2$representation(),  equals(rep_tr))
  expect_that(str2$representation(), equals(rep_str1))

  ## Did change the tree (yeah, this is a wonderful way of checking :P)
  expect_that(tr$representation(),
              equals(sub(str2$representation(),
                         other$representation(),
                         rep_tr, fixed=TRUE)))
  ## The full subtree
  expect_that(str$representation(),
              equals(sub(str2$representation(),
                         other$representation(),
                         rep_tr, fixed=TRUE)))
  ## And the child subtree
  expect_that(str1$representation(), equals(other$representation()))

  ## TODO: Confirm that "other" and the new tree are not linked.
})

test_that("subtree", {
  tr <- forest_tree(phy)
  cmp <- tr$representation()

  expect_that(tr$subtree("not_in_tree"),
              throws_error("Did not find node"))

  target <- "n8"
  sub2 <- tr$subtree(target)

  ## Modify the subtree:
  nd <- sub2$root_node()
  nd$label <- "subtree_root"
  sub2$set_root_node(nd)

  ## Test that propogates to the main tree:
  cmp2 <- sub(target, nd$label, cmp)
  expect_that(tr$representation(), is_identical_to(cmp2))

  ## For subtrees:
  expect_that(sub2$subtree("not_in_tree"),
              throws_error("Did not find node"))

  # Node exists, but not within this subtree, so error:
  expect_that(sub2$subtree("n6"),
              throws_error("Did not find node"))

  ## Can further get a subtree of a subtree:
  target2 <- "n9"
  sub3 <- sub2$subtree(target2)

  ## Modify the deepest subtree.
  nd3 <- sub3$root_node()
  nd3$label <- "subsubtree_root"
  sub3$set_root_node(nd3)

  ## Test that propogates to the main tree:
  cmp3 <- sub(target2, nd3$label, cmp2)
  expect_that(tr$representation(), is_identical_to(cmp3))

  ## Extract a further subtree from the main tree, but convert to full
  ## tree:
  sub4 <- tr$subtree("subtree_root")$to_tree()
  nd4 <- sub4$root_node()
  nd4$label <- "newtree_root"
  sub4$set_root_node(nd4)
  ## This change has *not* propagated up to the main tree:
  expect_that(tr$representation(), is_identical_to(cmp3))

  ## Terminal subtrees are fine.
  sub1 <- tr$subtree("t1")
  expect_that(sub1$representation(), equals("t1"))
  expect_that(sub1$count_tips(),  equals(1))
  expect_that(sub1$count_nodes(), equals(0))

  ## TODO: Replacement.
})
