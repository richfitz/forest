check_scalar <- function(x) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", deparse(substitute(x))))
  }
}
