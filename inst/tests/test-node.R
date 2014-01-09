source("helper-forest.R")

context("General node")

test_that("Label-less constructor", {
  nd <- new(xnode, 1)
  expect_that(nd$data,   is_identical_to(1L))
  expect_that(nd$label,  is_identical_to(""))
  expect_that(nd$length, is_identical_to(NA_real_))
})

test_that("With-label constructor", {
  nd <- new(xnode, 1, "lab")
  expect_that(nd$data,   is_identical_to(1L))
  expect_that(nd$label,  is_identical_to("lab"))
  expect_that(nd$length, is_identical_to(NA_real_))
})

test_that("With-edge-length constructor", {
  nd <- new(xnode, 1, "lab", pi)
  expect_that(nd$data,   is_identical_to(1L))
  expect_that(nd$label,  is_identical_to("lab"))
  expect_that(nd$length, is_identical_to(pi))
})

test_that("Node modification", {
  nd <- new(xnode, 1, "lab")
  expect_that(nd$data,  is_identical_to(1L))
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
  data1 <- 1L
  data2 <- 2L
  label1 <- "lab1"
  label2 <- "lab2"
  length1 <- pi
  length2 <- pi * 2

  nd1 <- new(xnode, data1, label1, length1)
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

  nd1 <- new(xnode, data1, label1, length1)
  nd2 <- new(xnode, data2, label2, length2) # differs
  nd3 <- new(xnode, data1, label1, length1) # same contents, different node
  nd4 <- new(xnode, data1, label2, length1) # one mismatch
  nd5 <- new(xnode, data2, label1, length1) # one mismatch
  nd6 <- new(xnode, data2, label1, length2) # one mismatch

  expect_that(nd1$equals(nd1), is_true())
  expect_that(nd1$equals(nd2), is_false())
  expect_that(nd1$equals(nd3), is_true())
  expect_that(nd1$equals(nd4), is_false())
  expect_that(nd1$equals(nd5), is_false())
  expect_that(nd1$equals(nd6), is_false())

  # No-edge-length comparison
  nd7 <- new(xnode, 1)
  nd8 <- new(xnode, 1)
  expect_that(is.na(nd7$length) && nd7$equals(nd8), is_true())
})
