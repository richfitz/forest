source("helper-forest.R")

context("Read Newick tree")

digits <- 10 # ape default

test_that("Read easy tree, no branch lengths", {
  str <- "(a,(b,c)i)r;"
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr)))
  expect_that(str, is_identical_to(to_newick_string(tr, digits)))
})

test_that("Read tree with no node labels", {
  str <- "(a,(b,c));"
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr)))
  expect_that(str, is_identical_to(to_newick_string(tr, digits)))
})

test_that("Read tree with edge length", {
  str <- "(a:1.352452,(b,c));"
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr)))
  expect_that(str, is_identical_to(to_newick_string(tr, digits)))
})

test_that("Read ape::rtree() tree", {
  set.seed(1)
  phy <- rtree(10)
  str <- write.tree(phy)
  tr <- read.newick(str)
  expect_that(str, is_identical_to(write.newick(tr, digits)))
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

  ## ape rips through and reads these all in .4s, writes back out in
  ## .32 -- nice and fast!
  phy.ape    <- lapply(files, ape::read.tree)
  newick.ape <- lapply(phy.ape, write.tree)

  ## Then do it with forest.  It takes 9.4s to read the full set (so
  ## 24x slower than ape)
  oo <- options(warn=2) # fail for any warning
  phy    <- lapply(str, function(x) try(read.newick(x)))
  options(oo)
  expect_that(any(sapply(phy, inherits, "try-error")), is_false())
  newick <- lapply(phy, write.newick)

  system.time(newick <- lapply(phy, write.newick))
  expect_that(newick, is_identical_to(newick.ape))

  ## This writes out the trees in 0.01s; so about 26x faster than
  ## ape.  Not bad!
  system.time(newick2 <- lapply(phy, to_newick_string, digits))
  expect_that(newick2, is_identical_to(newick.ape))
}
