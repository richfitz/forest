source("helper-forest.R")

context("Classify")
classify <- forest:::classify # hidden for now.

set.seed(1)
phy <- rtree(10)
phy$node.label <- paste0("n", seq_len(phy$Nnode))
tr <- forest.from.ape(phy)

# For comparisons.  This is always the order we expect things in
# (pre-order traversal)
label <- unlist(treeapply(tr, function(x) x$label))
## TODO: treeapply won't work for subtrees, because it can't take a
## subtree as an argument properly and simply crashes.  That's going
## to be entertaining to fix.

test_that("Classification of empty tree fails", {
  expect_that(classify(tr, character(0)),
              equals(structure(rep(0L, tr$size), names=label)))
})

test_that("Classification of a single clade", {
  classify1.manual <- function(nd) {
    base <- structure(rep(0L, tr$size), names=label)
    sub.n5 <- tr$get_subtree(nd)
    base[c(sub.n5$tip_labels, sub.n5$node_labels)] <- 1L
    base
  }

  for (i in label) {
    expect_that(classify(tr, i), is_identical_to(classify1.manual(i)))
  }
})

## TODO: Multi-regime cases.
test_that("Classification of a pair of clades", {
  expect_that(classify(tr, c("n5", "n6")),
              throws_error())
})
