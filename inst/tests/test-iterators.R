source("helper-forest.R")

context("Iterators")

vector_double <- forest:::vector_double

# First, test that the container works.  This is not that interesting
# as this is not really something that we export for use.
test_that("Empty vector is empty", {
  v <- new(vector_double)
  expect_that(v$size, equals(0))
  expect_that(v$empty, is_true())

  v$reserve(50L)
  expect_that(v$capacity, equals(50))

  vals <- 1:10
  v$assign(vals)
  expect_that(v$size, equals(length(vals)))
  expect_that(v$as.vector(), equals(vals))
  idx <- 4
  expect_that(v[[idx]], equals(vals[[idx]])) # R offset not working
  new.value <- -4
  v[[idx]] <- new.value
  expect_that(v[[idx]], equals(new.value))

  expect_that(v[[0L]], throws_error())
  expect_that(v[[length(vals) + 1L]], throws_error())
  expect_that(v[[0L]] <- 1, throws_error())
  expect_that(v[[length(vals) + 1L]] <- 1, throws_error())

  v$clear()
  expect_that(v$size, equals(0))
})

# avoiding a Rcpp weirdness about "no function to return from, jumping
# to top level" that seems to be triggered when 'v' is cleaned up
# automatically?
# possibly related to this:
# https://stat.ethz.ch/pipermail/r-devel/2011-December/062917.html
gc()

v <- new(vector_double)
v$assign(1:10)

test_that("Beginning iterator is sane", {
  it <- v$begin()
  expect_that(it$equals(v$begin()), is_true())
  expect_that(it$equals(v$end()),   is_false())
})

test_that("Copied iterators are equivalent but independent", {
  it <- v$begin()
  it2 <- it$copy()
  expect_that(it2$equals(it), is_true())
  it2$increment()
  expect_that(it2$equals(it), is_false())
  it$increment()
  expect_that(it2$equals(it), is_true())
})

test_that("Dereferencing iterators works", {
  it <- v$begin()
  vals <- v$as.vector()
  expect_that(it$value, is_identical_to(vals[[1L]]))
  it$increment() # now points at second element
  expect_that(it$value, is_identical_to(vals[[2L]]))

  new.value <- -2.0
  it$assign(new.value)
  expect_that(it$value, is_identical_to(new.value))
  # Also changed underlying data:
  expect_that(v[[2L]],  is_identical_to(new.value))

  # And the reverse -- change in the underlying data
  new.value <- -3.0
  v[[2L]] <- new.value
  expect_that(it$value, is_identical_to(new.value))
  expect_that(v[[2L]],  is_identical_to(new.value))

  it$decrement()
  expect_that(it$value, is_identical_to(vals[[1]]))
  expect_that(it$equals(v$begin()), is_true())
})

test_that("pre and post increment", {
  v2 <- new(vector_double)
  v2$assign(1:10)

  it <- v2$begin()
  tmp <- it$pre_increment()
  expect_that(tmp$value, equals(2))
  expect_that(it$value,  equals(2))

  tmp <- it$post_increment()
  expect_that(tmp$value, equals(2))
  expect_that(it$value,  equals(3))

  it$advance(3) # so, at "6"
  tmp <- it$post_decrement()
  expect_that(tmp$value, equals(6))
  expect_that(it$value,  equals(5))

  tmp <- it$pre_decrement()
  expect_that(tmp$value, equals(4))
  expect_that(it$value,  equals(4))

  ## And again, but dereferencing
  it <- v2$begin()
  tmp <- it$pre_increment()$value
  expect_that(tmp,       equals(2))
  expect_that(it$value,  equals(2))

  tmp <- it$post_increment()$value
  expect_that(tmp,       equals(2))
  expect_that(it$value,  equals(3))

  it$advance(3) # so, at "6"
  tmp <- it$post_decrement()$value
  expect_that(tmp,       equals(6))
  expect_that(it$value,  equals(5))

  tmp <- it$pre_decrement()$value
  expect_that(tmp,       equals(4))
  expect_that(it$value,  equals(4))
})

test_that("std::find works", {
  ## Pardon the ugly name for now:
  it <- forest:::find_vector_double_iterator(v$begin(), v$end(), 5)
  expect_that(it$equals(v$end()), is_false())
  expect_that(it$value, equals(5))

  ## Now, try and find something not in the vector:
  it <- forest:::find_vector_double_iterator(v$begin(), v$end(), 5.5)
  expect_that(it$equals(v$end()), is_true())

  ## Try getting this done a bit more nicely; I'm using "locate" here,
  ## because "find" is already used.  If these turn out to be useful,
  ## I'll try and get some more definitions written up.
  setGeneric("locate", function(begin, end, value) {
    standardGeneric("locate")
  }, where=.GlobalEnv)
  setMethod("locate",
            c(begin=forest:::vector_double_iterator,
              end=forest:::vector_double_iterator),
            function(begin, end, value)
            forest:::find_vector_double_iterator(begin, end, value))

  it1 <- forest:::find_vector_double_iterator(v$begin(), v$end(), 5)
  it2 <- locate(v$begin(), v$end(), 5)
  expect_that(it1$equals(it2), is_true())
})

test_that("std::distance works", {
  distance <- forest:::distance_vector_double_iterator
  expect_that(distance(v$begin(), v$end()),
              equals(v$size))

  it1 <- v$begin()
  it2 <- v$end()
  n1 <- 2
  n2 <- 1
  it1$advance(n1)
  it2$advance(-n2)
  expect_that(distance(it1, it2), equals(v$size - (n1 + n2)))
  expect_that(distance(it2, it1), equals(-distance(it1, it2)))
})

## It's easy to invalidate iterators by changing the underlying data.
## For example, run this under valgrind:
##
##   v <- new(vector_double)
##   v$assign(1:10)
##   it <- v$begin()
##   v$reserve(20)
##   it$value # invalid read here; iterator no longer valid.
##
## It does not seem possible to easily detect if an iterator is valid,
## so there's not that much that can be done.  This does mean that
## used incorrectly iterators will crash R.  So the solution is
## probably just not to expose them too much to users, even if they
## get used a bit internally.

## Another Rcpp/methods/gc issue:
gc()
