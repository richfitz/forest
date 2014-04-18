# Some vector graphics tricks.

##' Read in a vector file as a \code{grImport::Picture} object.
##'
##' The function \code{vector_read} is a high level wrapper that
##' attempts to do some trickery to most efficiently load the correct
##' file.  If the filename given ends in \code{.svg}, \code{.eps} or
##' \code{eps.xml} it will convert along to \code{eps.xml} and then
##' load.  If the filename has no extension (i.e., it is the part
##' \emph{up to} the extension, then it will load the \code{.svg},
##' \code{.eps} or \code{eps.xml} file, in increasing order of
##' preference, converting to \code{eps.xml} as it does so.  As such,
##' the second use of the function in this way will directly load the
##' \code{eps.xml} file.
##'
##' An additional step may be added to this process that converts all
##' the way to an rds representation of these xml files.  This can be
##' quite a bit faster, though potentially less reliable.
##'
##' @title Load Vector Graphics as a Picture Object
##' @param filename filename of a vector object in svg, eps, or
##' eps.xml format.
##' @author Rich FitzJohn
##' @export
vector_read <- function(filename) {
  assert_scalar(filename)
  if (grepl("\\.svg$", filename)) {
    vector_read_svg(filename)
  } else if (grepl("\\.eps$", filename)) {
    vector_read_svg(filename)
  } else if (grepl("\\.eps.xml", filename)) {
    vector_read_xml(filename)
  } else { # Assume we're given a basename and read the highest level
    if (file.exists(paste0(filename, ".eps.xml"))) {
      vector_read_xml(paste0(filename, ".eps.xml"))
    } else if (file.exists(paste0(filename, ".eps"))) {
      vector_read_eps(file.exists(paste0(filename, ".eps")))
    } else if (file.exists(paste0(filename, ".svg"))) {
      vector_read_svg(paste0(filename, ".svg"))
    } else {
      stop("Unable to load vector file")
    }
  }
}

inkscape_path <- function() {
  # OS X only for now, and hard coded unfortunately.  Should not be
  # too hard to sort out later.
  "/Applications/Inkscape.app/Contents/Resources/bin/inkscape"
}

# This currently does not even attempt to do this nicely; information
# will be printed to the screen, filenames and paths are not
# sanitised.  And because it's passing through system it's prone to
# all sorts of potential for mischief.
inkscape_svg_to_eps <- function(filename_svg, filename_eps) {
  system(sprintf("%s -f %s -E %s",
                 inkscape_path(), filename_svg, filename_eps))
}

##' @export
##' @rdname vector_read
vector_read_xml <- function(filename) {
  grImport::readPicture(filename)
}

##' @export
##' @rdname vector_read
vector_read_eps <- function(filename, retrace=FALSE) {
  filename_xml <- sprintf("%s.xml", filename)
  filename_tmp <- sprintf("capture%s", basename(filename))
  if (!file.exists(filename_xml) || retrace) {
    grImport::PostScriptTrace(filename, filename_xml)
    file.remove(filename_tmp)
  }
  vector_read_xml(filename_xml)
}

##' @export
##' @rdname vector_read
vector_read_svg <- function(filename, retrace=TRUE) {
  filename_eps <- paste0(tools::file_path_sans_ext(filename), ".eps")
  inkscape_svg_to_eps(filename, filename_eps)
  vector_read_eps(filename_eps, retrace)
}

# Take a vector image and colour it up.  This might come in useful as
# a high level function, or as low level via style_tree.
colour_picture <- function(picture, col) {
  assert_picture(picture)
  for (i in seq_along(picture@paths))
    picture@paths[[i]]@rgb <- col
  picture
}
