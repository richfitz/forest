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
    if (tr$is_node_type(x))
      tr$prepend(x)
    else
      tr$prepend_subtree(if (inherits(x, "tree.generator")) x() else x)
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

get.harmon.trees <- function(regenerate=FALSE) {
  path <- "harmon-2010-trees"

  if (file.exists(path)) {
    if (regenerate)
      unlink(path, recursive=TRUE)
    else
      return(path)
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

  return(path)
}
