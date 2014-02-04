source("helper-forest.R")

## So, the big issue here is that we can't address the matrix by
## rowname or column name.  So the association will need to be done
## some other way.  Doing a conversion to a list with names
## corresponding to tip names seems like the plan.

## That can easily be done in C++ of course.  What are we trying to
## achive here?

## Ideally we can do something like associating data to a tree; that
## will involve just putting data into an arbitrary slot within the
## "data" slot of the tree.  I'm not sure that there is any need to do
## this any other way than just dump it directly *into* that slot.

## This will probably end up with a better name soon
context("Associating tree and data")

set.seed(1)
phy <- rtree(10)
phy$node.label <- paste0("n", seq_len(phy$Nnode))
tr <- forest.from.ape(phy)

## This test might move into tree, and it might be lost entirely (and
## the tests done via the associating functions themselves).
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

## There is probably more testing to do here.
##
## Note that associating data will set the unset data to NULL.  This
## has some issues for the current implementation of treeapply; this
## test will break as soon as that function is changed or improved.
test_that("associate_data", {
  states.tips <- structure(as.list(runif(tr$tips)),
                           names=tr$tip_labels)
  states.nodes <- structure(as.list(runif(tr$nodes)),
                            names=tr$node_labels)
  states.all <- c(states.tips, states.nodes)

  tr$associate_data(states.tips, TRUE, FALSE)
  labels <- unlist(treeapply(tr, function(x) x$label))
  cmp <- structure(treeapply(tr, function(x) x$data), names=labels)
  expect_that(cmp[!sapply(cmp, is.null)], equals(states.tips))

  expect_that(tr$associate_data(2*states.tips[-1], TRUE, FALSE),
              throws_error())
  ## Did not change any data
  cmp <- structure(treeapply(tr, function(x) x$data), names=labels)
  expect_that(cmp[!sapply(cmp, is.null)], equals(states.tips))

  cmp <- structure(treeapply(tr, function(x) x$data), names=labels)
  expect_that(cmp[!sapply(cmp, is.null)], equals(states.tips))

  tr$associate_data(states.nodes, FALSE, TRUE)
  cmp <- structure(treeapply(tr, function(x) x$data), names=labels)
  expect_that(cmp[!sapply(cmp, is.null)], equals(states.nodes))

  tr$associate_data(states.all, TRUE, TRUE)
  cmp <- structure(treeapply(tr, function(x) x$data), names=labels)
  expect_that(cmp[names(states.nodes)], equals(states.nodes))
})

test_that("copy_structure", {
  states.tips <- structure(as.list(runif(tr$tips)),
                           names=tr$tip_labels)
  states.nodes <- structure(as.list(runif(tr$nodes)),
                            names=tr$node_labels)
  states.all <- c(states.tips, states.nodes)
  tr$associate_data(states.all, TRUE, TRUE)

  tr2 <- tr$copy_structure()
  expect_that(tr2$representation, is_identical_to(tr$representation))
  expect_that(to.newick.string(tr2),
              is_identical_to(to.newick.string(tr)))

  data1 <- structure(treeapply(tr, function(x) x$data),
                     names=unlist(treeapply(tr, function(x) x$label)))
  expect_that(data1[names(states.all)], equals(states.all))

  data2 <- treeapply(tr2, function(x) x$data)
  expect_that(all(sapply(data2, is.null)), is_true())
})

gc()
