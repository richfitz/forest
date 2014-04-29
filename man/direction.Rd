\name{direction}
\alias{direction}
\title{Tree Plot Direction}
\usage{
direction(direction, theta0 = 0)
}
\arguments{
  \item{direction}{A character vector (scalar) indicating
  the direction.  Must be one of "right", "left", "up",
  "down", "circle" or "semicircle"}

  \item{theta0}{Initial angle when making a semicircle
  plot.}
}
\description{
Helper function for setting up plotting directions.
}
\details{
This function is mostly used internally, but is useful for
circular plots that need to start at an angle different to
zero. It might eventually hold more basic layout options,
such as curly brackets.

This might change a little to become "shape", in which case
we'd have rectangular (with direction l, r, u, d),
semicircle with the same directions, circle with any
direction, etc.  Everything can change...

Using this function can result in daft looking things like
\code{direction=direction(d)} or even
\code{direction=direction(direction)}, but it generally
works thanks to R's scoping rules.  However, this function
might need to change name to make things clearer.
}
\author{
Rich FitzJohn
}

