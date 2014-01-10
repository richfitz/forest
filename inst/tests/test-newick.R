source("helper-forest.R")

test_that("Read easy tree, no branch lengths", {
  str <- "(a,(b,c)i)r;"
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr)))
})

test_that("Read tree with no node labels", {
  str <- "(a,(b,c));"
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr)))
})

test_that("Read tree with edge length", {
  str <- "(a:1.352452,(b,c));"
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr)))
})

test_that("Read ape::rtree() tree", {
  set.seed(1)
  phy <- rtree(10)
  str <- write.tree(phy)
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr)))
})

# This is disabled because it is slow, and because it depends on a
# bunch of trees from Harmon et al. 2010:
if (FALSE) {
  path <- "harmon-2010-trees"
  files <- dir(path, pattern="phy$", full.names=TRUE)

  # Quick fix on some braindead formatting here:
  str <- readLines(file.path(path, "geospiza.phy"))
  if (grepl(" ", str))
    writeLines(gsub(" ", "", str), file.path(path, "geospiza.phy"))

  ## Read in all trees:
  str <- suppressWarnings(lapply(files, readLines))

  ## ape rips through and reads these all in .4s -- nice and fast!
  phy.ape    <- lapply(files, ape::read.tree)
  newick.ape <- lapply(phy.ape, write.tree)

  ## Then do it with forest.  It takes 9.4s to read the full set (so
  ## 24x slower than ape)
  oo <- options(warn=2) # fail for any warning
  phy    <- lapply(str, function(x) try(read.newick(x)))
  options(oo)
  expect_that(any(sapply(phy, inherits, "try-error")), is_false())
  newick <- lapply(phy, write.newick)

  expect_that(newick, is_identical_to(newick.ape))
}
