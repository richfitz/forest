source("helper-forest.R")

## So, the big issue here is that we can't address the matrix by
## rowname or column name.  So the alignment will need to be done some
## other way.  Doing a conversion to a list with names corresponding
## to tip names seems like the plan.

## That can easily be done in C++ of course.  What are we trying to
## achive here?

## Ideally we can do something like aligning data to a tree; that will
## involve just putting data into an arbitrary slot within the "data"
## slot of the tree.  I'm not sure that there is any need to do this
## any other way than just dump it directly *into* that slot.

## This will probably end up with a better name soon
context("Align tree and data")

set.seed(1)
phy <- rtree(10)
phy$node.label <- paste0("n", seq_len(phy$Nnode))
tr <- forest.from.ape(phy)

## This test might move into tree, and it might be lost entirely
## (and move within the alignment functions themselves).
test_that("check_names", {
  ## Corner cases:
  expect_that(tr$check_names(character(0), TRUE,  TRUE),  is_false())
  expect_that(tr$check_names(character(0), FALSE, FALSE), is_true())

  tip.labels  <- tr$tip_labels
  node.labels <- tr$node_labels
  all.labels  <- c(tip.labels, node.labels)

  expect_that(tr$check_names(tip.labels,  TRUE, FALSE), is_true())
  expect_that(tr$check_names(tip.labels,  FALSE, TRUE), is_false())
  expect_that(tr$check_names(tip.labels,  TRUE, TRUE),  is_false())

  expect_that(tr$check_names(node.labels, TRUE, FALSE), is_false())
  expect_that(tr$check_names(node.labels, FALSE, TRUE), is_true())
  expect_that(tr$check_names(node.labels, TRUE, TRUE),  is_false())

  expect_that(tr$check_names(all.labels,  FALSE, TRUE), is_true())
  expect_that(tr$check_names(all.labels,  TRUE, FALSE), is_true())
  expect_that(tr$check_names(all.labels,  TRUE, TRUE),  is_true())

  ## Missing a single name fails:
  expect_that(tr$check_names(tip.labels[-1],  TRUE, FALSE), is_false())
  expect_that(tr$check_names(node.labels[-1], FALSE, TRUE), is_false())
  expect_that(tr$check_names(all.labels[-1],  TRUE,  TRUE), is_false())
})
