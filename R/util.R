check_scalar <- function(x) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", deparse(substitute(x))))
  }
}

assert_scalar <- function(x) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", deparse(substitute(x))))
  }
}

assert_unit <- function(x) {
  if (!is.unit(x)) {
    stop(sprintf("%s must be a unit", deparse(substitute(x))))
  }
}

assert_number <- function(x) {
  if (!(is.numeric(x) || is.integer(x))) {
    stop(sprintf("%s must be numeric", deparse(substitute(x))))
  }
}

assert_grob <- function(x) {
  if (!is.grob(x)) {
    stop(sprintf("%s must be a grob", deparse(substitute(x))))
  }
}

assert_picture <- function(x) {
  if (!inherits(x, "Picture")) {
    stop(sprintf("%s must be a Picture", deparse(substitute(x))))
  }
}

assert_file_exists <- function(x) {
  if (!file.exists(x)) {
    stop(sprintf("file %s does not exist", x))
  }
}