\name{vector_read}
\alias{vector_read}
\alias{vector_read_eps}
\alias{vector_read_svg}
\alias{vector_read_xml}
\title{Load Vector Graphics as a Picture Object}
\usage{
vector_read(filename)

vector_read_xml(filename)

vector_read_eps(filename, retrace = FALSE)

vector_read_svg(filename, retrace = TRUE)
}
\arguments{
  \item{filename}{filename of a vector object in svg, eps,
  or eps.xml format.}
}
\description{
Read in a vector file as a \code{grImport::Picture} object.
}
\details{
The function \code{vector_read} is a high level wrapper
that attempts to do some trickery to most efficiently load
the correct file.  If the filename given ends in
\code{.svg}, \code{.eps} or \code{eps.xml} it will convert
along to \code{eps.xml} and then load.  If the filename has
no extension (i.e., it is the part \emph{up to} the
extension, then it will load the \code{.svg}, \code{.eps}
or \code{eps.xml} file, in increasing order of preference,
converting to \code{eps.xml} as it does so.  As such, the
second use of the function in this way will directly load
the \code{eps.xml} file.

An additional step may be added to this process that
converts all the way to an rds representation of these xml
files.  This can be quite a bit faster, though potentially
less reliable.
}
\author{
Rich FitzJohn
}

