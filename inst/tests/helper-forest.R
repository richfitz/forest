library(methods)
library(Rcpp)
library(testthat)
library(forest)
library(ape)

is_expected_tree <- function(n, representation) {
  function(tr) {
    ok <- (isTRUE(all.equal(tr$size, n))           &&
           isTRUE(all.equal(tr$empty, n == 0))     &&
           isTRUE(all.equal(tr$childless, n <= 1)) &&
           isTRUE(all.equal(tr$representation, representation)))
    expectation(ok, "Tree does not have expected contents")
  }
}

is_same_tree_as <- function(cmp) {
  function(tr)
    expectation(tr$equals(cmp), "Trees differ")
}

is_different_tree_to <- function(cmp) {
  function(tr)
    expectation(!tr$equals(cmp), "Trees are the same")
}

make.tree_of <- function(class) {
  prepend <- function(tr, x) {
    if (inherits(x, "tree.generator"))
      tr$prepend_subtree(x())
    else if (class(x) == class(tr))
      tr$prepend_subtree(x)
    else
      tr$prepend(x)
  }

  function(v, ...) {
    tr <- new(class, v)

    rec <- function(...) {
      tail <- list(...)
      if (length(tail) == 0)
        return(tr)
      for (i in rev(tail))
        prepend(tr, i)
      rec
    }

    class(rec) <- "tree.generator"
    rec
  }
}

get.harmon.trees <- function(path, regenerate=FALSE) {
  if (file.exists(path)) {
    if (regenerate)
      unlink(path, recursive=TRUE)
    else
      return(invisible(path))
  }

  zipfile <- "harmon2010.zip"
  url <-
    "http://datadryad.org/bitstream/handle/10255/dryad.55049/Harmonetal2010.zip?sequence=1"
  download.file(url, zipfile)

  unlink(path, recursive=TRUE)
  dir.create(path, FALSE)
  unzip(zipfile, exdir=path)
  keep <- dir(file.path(path, "data"), pattern="\\.phy$")
  file.rename(file.path(path, "data", keep),
              file.path(path, keep))

  unlink(file.path(path, "__MACOSX"), recursive=TRUE)
  unlink(file.path(path, "data"), recursive=TRUE)
  file.remove(file.path(path, "rates_data_nonwp_2.R"))

  file.remove(zipfile)

  # Quick fix on some braindead formatting within a number here:
  str <- readLines(file.path(path, "geospiza.phy"))
  if (grepl(" ", str))
    writeLines(gsub(" ", "", str), file.path(path, "geospiza.phy"))

  invisible(path)
}

## Very hackish version of a treeapply() type function.  Pre-order
## traversal only, and taking node only (not subtree, or data)
treeapply <- function(tr, f) {
  lapply(forest:::drain_tree(tr), f)
}

make.node.builder.xnode <- function(phy) {
  label  <- c(phy$tip.label, phy$node.label)
  idx    <- seq_len(phy$Nnode + Ntip(phy))
  length <- phy$edge.length[match(idx, phy$edge[,2])]
  if (is.null(length))
    length <- rep(NA_real_, length(label))
  function(i)
    new(xnode, label[[i]], length[[i]], as.numeric(i))
}

## Copied over from diversitree, for now:
## Similar to ape's branching.times(), but returns the height above
## the root node, even for non-ultrametric trees.  Includes tip times.
dt_branching.heights <- function(phy) {
  if (!inherits(phy, "phylo"))
    stop('object "phy" is not of class "phylo"')
  phy <- reorder(phy, "cladewise")

  edge <- phy$edge
  n.node <- phy$Nnode
  n.tip <- length(phy$tip.label)

  ht <- numeric(n.node + n.tip) # zero'd
  for (i in seq_len(nrow(edge)))
    ht[edge[i, 2]] <- ht[edge[i, 1]] + phy$edge.length[i]

  ## Ugly, but fairly compatible with branching.times()
  names.node <- phy$node.label
  if (is.null(names.node))
    names.node <- (n.tip + 1):(n.tip + n.node)
  names(ht) <- c(phy$tip.label, names.node)

  ht
}
