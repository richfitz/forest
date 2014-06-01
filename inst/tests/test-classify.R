source("helper-forest.R")

## TODO: This is failing because classify takes 'tr' directly, but
## that does not give the actual pointer.  So we need to tweak the
## 'as' function to grab the pointer if given an object that is not an
## external pointer.  There are ways of testing for external
## pointerness that should make this easy.

context("Classify")
classify <- forest:::classify # hidden for now.

set.seed(1)
phy <- rtree(10)
phy$node.label <- paste0("n", seq_len(phy$Nnode))
tr <- forest_tree(phy)

# For comparisons.  This is always the order we expect things in
# (pre-order traversal)
label <- unlist(treeapply(tr, function(x) x$label))
## TODO: treeapply won't work for subtrees, because it can't take a
## subtree as an argument properly and simply crashes.  That's going
## to be entertaining to fix.

## TODO: classification of empty tree

test_that("Trivial classification", {
  expect_that(classify(tr, character(0)),
              equals(structure(rep(0L, tr$size()), names=label)))
})

test_that("Classification of a single clade", {
  classify1.manual <- function(nd) {
    base <- structure(rep(0L, tr$size()), names=label)
    sub <- tr$subtree(nd)
    base[c(sub$tip_labels(), sub$node_labels())] <- 1L
    base
  }

  for (i in label) {
    expect_that(classify(tr, i), is_identical_to(classify1.manual(i)))
  }
})

## TODO: Multi-regime cases.
test_that("Classification of a pair of clades", {
  classify.manual <- function(nd) {
    base <- structure(rep(0L, tr$size()), names=label)
    for (i in nd) {
      sub <- tr$subtree(i)
      j <- c(sub$tip_labels(), sub$node_labels())
      base[j][base[j] == base[[sub$root_node()$label]]] <- max(base) + 1L
    }
    base
  }

  ## TODO: This is quite slow (1.5s), but I like that it's
  ## exhaustive.  The slowness is almost entirely the .manual
  ## version and a good chunk of that is the various overheads (Rcpp
  ## and S4).  There are 384 tests being done here, so that's a lot!
  for (i in label) {
    for (j in label) {
      if (i == j) {
        expect_that(classify(tr, c(i, j)), throws_error())
      } else {
        expect_that(classify(tr, c(i, j)),
                    equals(classify.manual(c(i, j))))
      }
    }
  }
})
