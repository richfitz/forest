source("helper-forest.R")

context("General node")

test_that("Label-less constructor", {
  nd <- new(xnode, 1)
  expect_that(nd$data,  is_identical_to(1L))
  expect_that(nd$label, is_identical_to(""))
})

test_that("With-label constructor", {
  nd <- new(xnode, 1, "lab")
  expect_that(nd$data,  is_identical_to(1L))
  expect_that(nd$label, is_identical_to("lab"))
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
})

test_that("Node copy", {
  data1 <- 1L
  data2 <- 2L
  label1 <- "lab1"
  label2 <- "lab2"

  nd1 <- new(xnode, data1, label1)
  nd2 <- nd1$copy()

  expect_that(nd2$data,  is_identical_to(data1))
  expect_that(nd2$label, is_identical_to(label1))

  nd2$data  <- data2
  nd2$label <- label2

  # Original unchanged:
  expect_that(nd1$data,  is_identical_to(data1))
  expect_that(nd1$label, is_identical_to(label1))
  # New version modified
  expect_that(nd2$data,  is_identical_to(data2))
  expect_that(nd2$label, is_identical_to(label2))
})

test_that("Node equality", {
  data1 <- 1L
  data2 <- 2L
  label1 <- "lab1"
  label2 <- "lab2"

  nd1 <- new(xnode, data1, label1)
  nd2 <- new(xnode, data2, label2) # differs
  nd3 <- new(xnode, data1, label1) # same contents, different node
  nd4 <- new(xnode, data1, label2) # one mismatch
  nd5 <- new(xnode, data2, label1) # other mismatch

  expect_that(nd1$equals(nd1), is_true())
  expect_that(nd1$equals(nd2), is_false())
  expect_that(nd1$equals(nd3), is_true())
  expect_that(nd1$equals(nd4), is_false())
  expect_that(nd1$equals(nd5), is_false())
})
