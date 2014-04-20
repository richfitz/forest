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

##' @importFrom tools file_path_sans_ext
replace_extension <- function(x, ext) {
  if (grepl("^[^.]", ext)) {
    ext <- paste0(".", ext)
  }
  paste0(file_path_sans_ext(x), ext)
}

system_which <- function(app) {
  assert_scalar(app)
  path <- suppressWarnings(system(paste("which", app), intern=TRUE))
  if (length(path) == 1) {
    path
  } else {
    NULL
  }
}
