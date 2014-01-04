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

# Should be able to test failure by reserving enough space so that the
# iterator reallocates -- end() will have changed.

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
