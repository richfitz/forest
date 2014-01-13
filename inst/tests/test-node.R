source("helper-forest.R")

context("General node")

test_that("Default constructor", {
  nd <- new(xnode)
  # This follows from default constructing an int.
  expect_that(nd$data,       is_identical_to(0L))
  expect_that(nd$label,      is_identical_to(""))
  expect_that(nd$length,     is_identical_to(NA_real_))
  expect_that(nd$has_label,  is_false())
  expect_that(nd$has_length, is_false())
})

test_that("With-label constructor", {
  nd <- new(xnode, "lab")
  expect_that(nd$data,       is_identical_to(0L))
  expect_that(nd$label,      is_identical_to("lab"))
  expect_that(nd$length,     is_identical_to(NA_real_))
  expect_that(nd$has_label,  is_true())
  expect_that(nd$has_length, is_false())
})

test_that("With-edge-length constructor", {
  nd <- new(xnode, "lab", pi)
  expect_that(nd$data,       is_identical_to(0L))
  expect_that(nd$label,      is_identical_to("lab"))
  expect_that(nd$length,     is_identical_to(pi))
  expect_that(nd$has_label,  is_true())
  expect_that(nd$has_length, is_true())
})

test_that("Node modification", {
  nd <- new(xnode, "lab")
  expect_that(nd$data,  is_identical_to(0L))
  new.data <- 5L
  nd$data <- new.data
  expect_that(nd$data, is_identical_to(new.data))
  expect_that(nd$data <- "incompatible type", throws_error())

  new.label <- "new label"
  nd$label <- new.label
  expect_that(nd$label, is_identical_to(new.label))
  # No type conversion, even though R would do it
  expect_that(nd$label <- new.data, throws_error())
  # And certainly none here
  expect_that(nd$label <- list("incompatible"), throws_error())

  new.length <- pi
  nd$length <- new.length
  expect_that(nd$length, is_identical_to(new.length))
  expect_that(nd$length <- "incompatible type", throws_error())
})

test_that("Node copy", {
  data1 <- 0L
  data2 <- 2L
  label1 <- "lab1"
  label2 <- "lab2"
  length1 <- pi
  length2 <- pi * 2

  nd1 <- new(xnode, label1, length1)
  nd1$data <- data1
  nd2 <- nd1$copy()

  expect_that(nd2$data,   is_identical_to(data1))
  expect_that(nd2$label,  is_identical_to(label1))
  expect_that(nd2$length, is_identical_to(length1))

  nd2$data   <- data2
  nd2$label  <- label2
  nd2$length <- length2

  # Original unchanged:
  expect_that(nd1$data,   is_identical_to(data1))
  expect_that(nd1$label,  is_identical_to(label1))
  expect_that(nd1$length, is_identical_to(length1))
  # New version modified
  expect_that(nd2$data,   is_identical_to(data2))
  expect_that(nd2$label,  is_identical_to(label2))
  expect_that(nd2$length, is_identical_to(length2))
})

test_that("Node equality", {
  data1 <- 1L
  data2 <- 2L
  label1 <- "lab1"
  label2 <- "lab2"
  length1 <- pi
  length2 <- pi * 2

  ## Three nodes -- the second has different contents (so should be
  ## unequal) the second has the same contents, but created
  ## separately (so should be equal)
  nd1 <- new(xnode, label1, length1, data1)
  nd2 <- new(xnode, label2, length2, data2)
  nd3 <- new(xnode, label1, length1, data1)

  expect_that(nd1$equals(nd1), is_true())
  expect_that(nd1$equals(nd2), is_false())
  expect_that(nd1$equals(nd3), is_true())

  ## Three different ways of differing by a single component:
  nd4 <- new(xnode, label2, length1, data1) # label
  nd5 <- new(xnode, label1, length2, data1) # length
  nd6 <- new(xnode, label1, length1, data2) # data

  expect_that(nd1$equals(nd4), is_false())
  expect_that(nd1$equals(nd5), is_false())
  expect_that(nd1$equals(nd6), is_false())

  ## Nodes with no edge are equal with the same contents:
  nd7 <- new(xnode, label1); nd7$data <- data1
  nd8 <- new(xnode, label1); nd8$data <- data1
  expect_that(is.na(nd7$length) && !nd7$has_length && nd7$equals(nd8),
              is_true())

  ## Though they differ from nodes with an edge length:
  expect_that(nd7$equals(nd1), is_false())
})
