source("helper-forest.R")

context("Util (grid)")

test_that("corner cases for combine_gpars", {
  ## Can't have an empty list of graphics parameters:
  expect_that(forest:::combine_gpars(list(), integer(0)), throws_error())
  ## Indices too large:
  expect_that(forest:::combine_gpars(list(gpar()), c(1, 2)),
              throws_error("Invalid indices in index"))
  ## Indices too small
  expect_that(forest:::combine_gpars(list(gpar()), c(0, 1)),
              throws_error("Invalid indices in index"))

  ## TODO:
  ## Non-scalar graphics parameters
  ## Non-gpar list elements
})

test_that("expand out a single parameter", {
  list <- list(gpar(), gpar(col="red"))
  index <- c(1, 1, 2, 2)
  res <- forest:::combine_gpars(list, index)
  expect_that(names(res), equals("col"))
  expect_that(length(res$col), equals(length(index)))

  ## If only a single type is given, we just refuse to expand:
  expect_that(forest:::combine_gpars(list[1], c(1, 1)),
              is_identical_to(list[[1]]))
})

## TODO: Multi key cases.
## TODO: Inheritance of parameters from pushed viewports
