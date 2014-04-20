# Some vector graphics tricks.

##' Read in a vector file as a \code{grImport::Picture} object.
##'
##' This is a bit of a Rube Goldberg machine because of a disconnect
##' between how most vector images are likely to be found (SVG, e.g.,
##' \url{http://phylopic.org}) and the formats that R is capable of
##' reading.  There are a number of steps here that require external
##' programs, and cause potential platform non-indepenence.  The
##' process is:
##'
##' \enumerate{
##'   \item{Convert SVG to EPS.  This can be done with Inkscape
##'     automatically, or with other programs manually.  The fucntion
##'     \code{forest:::inkscape_svg_to_eps} function does this step,
##'     but requires Inkscape to be installed.}
##'   \item{Convert EPS to XML. \code{grImport::readPicture} has an
##'     internal XML format that it uses, called RGML.  To convert to
##'     this format, it uses ghostscript to process the EPS file and
##'     then does some post-processing.  The function
##'     \code{forest:::ghostscript_eps_to_xml} does this step, but
##'     requires ghostscript to be installed.}
##'   \item{Read XML into R.  This is done with
##'     \code{grImport::readPicture} and creates an object of class
##'     \code{Picture} that can be drawn using \code{grid.picture} and
##'     eventuall some forest functions.}
##' }
##'
##' The function \code{vector_read} is a high level wrapper to this
##' process that attempts to do as little work as possible.  This
##' means that if an XML file that corresponds to an EPS/SVG file
##' exists, that file will be read rather than going back and
##' recreating the XML file.  This means that not all of the
##' conversion software needs to installed if the processed files
##' exist.
##'
##' Alternatively, \code{vector_read_svg}, \code{vector_read_eps} and
##' \code{vector_read_xml} will do all the necessary processing and
##' read the resulting object in as a Picture.  They will
##' \emph{always} reprocess files though.
##'
##' The above functions use a set of conventions for filenames:
##' \code{picture.svg} becomes \code{picture.eps} becomes
##' \code{picture.xml}.  If the starting point is an eps file then
##' this is simply \code{picture.eps} becomes \code{picture.xml}.
##'
##' Reading in the XML files can be quite slow.  I may add an
##' additional step here that serialises the object as RDS and read
##' from the preferentially.  In this case the processed
##' \code{picture.xml} becomes \code{picture.rds}.
##'
##' @title Load Vector Graphics as a Picture Object
##' @param filename filename of a vector object in svg, eps, or
##' eps.xml format.
##' @author Rich FitzJohn
##' @export
vector_read <- function(filename) {
  assert_scalar(filename)
  filename_xml <- replace_extension(filename, "xml")
  filename_eps <- replace_extension(filename, "eps")

  if (file.exists(filename_xml)) {
    vector_read_xml(filename_xml)
  } else if (file.exists(filename_eps)) {
    vector_read_eps(filename_eps)
  } else if (grepl("\\.svg$", filename)) {
    vector_read_svg(filename)
  } else {
    stop("I don't think this is going to work out.")
  }
}

inkscape_path <- function() {
  # OS X only for now, and hard coded unfortunately.  Should not be
  # too hard to sort out later.
  #
  # For windows, I have *no* idea if this is going to be possible.
  #
  # Might be easiest to also check getOption("inkscape_path"), which
  # would allow manual setting, and fall back on this if nothing is
  # given.
  #
  # Perhaps look at some of the hoops that grImport::PostScriptTrace
  # goes through to get ghostscript located.
  if (.Platform$OS.type == "unix") {
    path <- system_which("inkscape")
    if (!is.null(path)) {
      return(path)
    } else {
      path.osx <- "/Applications/Inkscape.app/Contents/Resources/bin/inkscape"
      if (file.exists(path.osx)) {
        return(path.osx)
      }
    }
  } else if (.Platform$OS.type == "windows") {
    stop("Windows not yet supported: please see github issue #52")
  }
  stop("Failed to locate inkscape")
}

# This currently does not even attempt to do this nicely; information
# will be printed to the screen, filenames and paths are not
# sanitised.  And because it's passing through system it's prone to
# all sorts of potential for mischief.
#
# It also looks like on Windows we may have to fully specify paths
#   http://www.inkscapeforum.com/viewtopic.php?f=5&t=4191
#   https://bugs.launchpad.net/inkscape/+bug/504713
inkscape_svg_to_eps <- function(filename_svg, filename_eps, quiet=TRUE) {
  cmd <- sprintf("%s -f %s -E %s",
                 inkscape_path(), filename_svg, filename_eps)
  ret <- system(cmd, ignore.stderr=quiet)
  # This doesn't actually capture all errors (e.g., if eps file exists
  # already then this just sails on happily).
  if (ret != 0) {
    stop("There was an error running Inkscape with command ", cmd)
  }
  if (!file.exists(filename_eps)) {
    stop("Creating the eps file seems to have failed")
  }
}

ghostscript_eps_to_xml <- function(filename_eps, filename_xml) {
  filename_tmp <- sprintf("capture%s", basename(filename_eps))
  grImport::PostScriptTrace(filename_eps, filename_xml)
  file.remove(filename_tmp)
}

##' @export
##' @rdname vector_read
vector_read_xml <- function(filename) {
  assert_scalar(filename)
  grImport::readPicture(filename)
}

##' @export
##' @rdname vector_read
vector_read_eps <- function(filename) {
  assert_scalar(filename)
  filename_xml <- replace_extension(filename, "xml")
  ghostscript_eps_to_xml(filename, filename_xml)
  vector_read_xml(filename_xml)
}

##' @export
##' @rdname vector_read
vector_read_svg <- function(filename) {
  assert_scalar(filename)
  filename_eps <- replace_extension(filename, "eps")
  inkscape_svg_to_eps(filename, filename_eps)
  vector_read_eps(filename_eps)
}

##' Colour all paths in a vector image (class \code{Picture}) with a
##' single colour.  This is probably most useful for straight
##' silhouettes, rather than those with multiple colours.
##'
##' May change to allow matching on source colour in the future.
##' @title Colour a Vector Picture
##' @param picture A \code{Picture} object
##' @param col Colour to cange paths to
##' @author Rich FitzJohn
##' @export
colour_picture <- function(picture, col) {
  assert_picture(picture)
  for (i in seq_along(picture@paths))
    picture@paths[[i]]@rgb <- col
  picture
}
