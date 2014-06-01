source("helper-forest.R")

context("Node")

test_that("Default constructor", {
  nd <- forest_node()
  expect_that(nd$data,       is_identical_to(NULL))
  expect_that(nd$label,      is_identical_to(""))
  expect_that(nd$length,     is_identical_to(NA_real_))

  dat <- list(1,2)
  nd <- forest_node("lab", pi, dat)
  expect_that(nd$data,       is_identical_to(dat))
  expect_that(nd$label,      is_identical_to("lab"))
  expect_that(nd$length,     is_identical_to(pi))
})

test_that("Node modification & data sanitisation", {
  ## First, check that we can change the data, label and length.
  nd <- forest_node("lab")
  expect_that(nd$data,  is_identical_to(NULL))
  new_data <- 5L
  nd$data <- new_data
  expect_that(nd$data, is_identical_to(new_data))

  ## Weirder data
  new_data <- list(1, pi, 1:5)
  nd$data <- new_data
  expect_that(nd$data, is_identical_to(new_data))

  new.label <- "new label"
  nd$label <- new.label
  expect_that(nd$label, is_identical_to(new.label))

  ## Labels need to be scalar and character.
  expect_that(nd$label <- new_data,
              throws_error("value must be a scalar"))
  expect_that(nd$label <- pi,
              throws_error("value must be a character"))
  expect_that(nd$label <- list("incompatible"),
              throws_error("value must be a character"))

  new.length <- pi
  nd$length <- new.length
  expect_that(nd$length, is_identical_to(new.length))
  expect_that(nd$length <- "incompatible type",
              throws_error("must be numeric"))
  expect_that(nd$length <- -1,
              throws_error("must be nonnegative"))
  ## This is OK:
  nd$length <- NA_real_
  ## But this is not (perhaps it should be)
  expect_that(nd$length <- NA, throws_error("must be numeric"))

  ## Other access by index:
  expect_that(nd[["label"]],  is_identical_to(nd$label))
  expect_that(nd[["length"]], is_identical_to(nd$length))
  expect_that(nd[["data"]],   is_identical_to(nd$data))
  expect_that(nd["label"],  is_identical_to(nd$label))
  expect_that(nd["length"], is_identical_to(nd$length))
  expect_that(nd["data"],   is_identical_to(nd$data))

  ## Slightly cryptic error because this triggers recursive indexing
  ## for '[['.
  expect_that(nd[[c("label", "data")]],
              throws_error("subscript out of bounds"))
  expect_that(nd[c("label", "data")],
              throws_error("subscript out of bounds"))

  ## Other assignment:
  nd2 <- forest_node()
  nd2[["label"]]  <- nd$label
  nd2[["length"]] <- nd$length
  nd2[["data"]]   <- nd$data
  expect_that(nd2, is_identical_to(nd))

  nd3 <- forest_node()
  nd3["label"]  <- nd$label
  nd3["length"] <- nd$length
  nd3["data"]   <- nd$data
  expect_that(nd3, is_identical_to(nd))
})

test_that("node restricted assignents", {
  nd <- forest_node()

  ## First, check that multiple names will always fail:
  expect_that(nd[[c("label", "length")]] <- list("foo", pi),
              throws_error("name must be a scalar"))

  expect_that(nd$foo <- 1, throws_error("Invalid name"))
  expect_that(nd[["foo"]] <- 1, throws_error("Invalid name"))
  expect_that(nd["foo"] <- 1, throws_error("Invalid name"))
})
