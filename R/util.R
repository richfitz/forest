## TODO: Port rodeint's set of assertion functions here.  Might need
## to use a standard set at some point.
assert_scalar <- function(x) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", deparse(substitute(x))))
  }
}

assert_character <- function(x) {
  if (!is.character(x)) {
    stop(sprintf("%s must be a character", deparse(substitute(x))))
  }
}

assert_length <- function(x, expected_length) {
  if (length(x) != expected_length) {
    stop("%s must have length %d (recieved %d)",
         deparse(substitute(x)), expected_length, length(x))
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

assert_list <- function(x) {
  if (!is.list(x)) {
    stop(sprintf("%s must be a list", deparse(substitute(x))))
  }
}

assert_grob <- function(x) {
  if (!is.grob(x)) {
    stop(sprintf("%s must be a grob", deparse(substitute(x))))
  }
}

assert_picture <- function(x) {
  assert_inherits(x, "Picture")
}

assert_inherits <- function(x, what, name=deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("%s must be a %s", name,
                 paste(what, collapse=" / ")), call.=FALSE)
  }
}

assert_named <- function(x, empty_can_be_unnamed=TRUE) {
  if (is.null(names(x)) || any(names(x) == "")) {
    if (length(x) > 0 || !empty_can_be_unnamed) {
      stop(sprintf("%s must be named", deparse(substitute(x))))
    }
  }
}

assert_names_align <- function(x, target) {
  nx <- names(x)
  if (!all(target %in% nx)) {
    stop(sprintf("Missing names in %s: %s",
                 deparse(substitute(x)), paste(setdiff(target, nx))))
  }
  if (!all(nx %in% target)) {
    stop(sprintf("Unknown names in %s: %s",
                 deparse(substitute(x)), paste(setdiff(nx, target))))
  }
}

assert_file_exists <- function(x) {
  if (!file.exists(x)) {
    stop(sprintf("file %s does not exist", x))
  }
}

assert_nonnegative <- function(x, name=deparse(substitute(x))) {
  if (x < 0) {
    stop(sprintf("%s must be nonnegative", name), call.=FALSE)
  }
}

# It appears that rep_len or rep.int could be more efficient than rep
# here, but we'll use this with unit objects, and rep() is required
# for that to work properly (because otherwise dispatch doesn't
# happen).
recycle_simple <- function(x, n) {
  if (length(x) == 1) {
    x <- rep(x, length.out=n)
  } else if (length(x) != n) {
    if (n > 1) {
      stop(sprintf("%s must have length 1 or %d (recieved %d)",
                   deparse(substitute(x)), n, length(x)))
    } else {
      stop(sprintf("%s must have length %d (recieved %d)",
                   deparse(substitute(x)), n, length(x)))
    }
  }
  x
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
