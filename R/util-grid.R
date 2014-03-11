## Utilities for working with grid graphics.

##' Combine a list of graphical parameters.
##'
##' The idea is this; sometimes it makes sense to think of different
##' regions of a tree, or different lines, words, etc having different
##' graphical parameters, and we'd like to set them using the
##' \code{gpar()} function.  Something like
##' `list(base=gpar(col="black"), Asteraceae=gpar(col="yellow"))` and
##' then have these expanded out to a single \code{gpar} object with
##' the \code{col} element appropriately vectorised, so that the
##' branches, tip labels, node labels, etc for Asteraceae will end up
##' yellow while everything else remains black.
##'
##' This should work for any number of parameters, so that if
##' \code{lwd} is present in one list and not in the other, when
##' everything gets vectorised out, \code{lwd} is going to be needed
##' in both.  That means that we need to look up what the appropriate
##' \emph{default} value of \code{lwd} is going to be.  We do that by
##' using \code{get.gpar()} (if this is used during a
##' \code{drawDetails} method then this will behave most predictably,
##' otherwise it will probably end up with the grid defaults).
##'
##' Note that if no graphics window is open, this will open one.
##'
##' The first element of the list is considered to be the \emph{base}
##' parameters.
##'
##' This function does not do validation of parameters, or that we
##' have even got \code{gpar} objects at this point.  Also needing
##' validation: each element is a scalar.
##' @title Combine Graphical Parameters
##' @author Rich FitzJohn
##' @param list An R list (not a \code{gpar} list), each element of
##' which is a \code{gpar} list).  The first element is taken as the
##' "base" parameters.
##' @param index
combine_gpars <- function(list, index) {
  ## TODO: If something is invariant, then record it as a scalar;
  ## don't expand out.
  ##
  ## TODO: This function is too weird.  Too hard to reason about.  I
  ## may have the wrong end of the stick here.
  if (length(list) < 1)
    stop("Need at least a base set of graphics parameters")
  if (min(index) < 1 || max(index) > length(list))
    stop("Invalid indices in index")
  list <- lapply(list, check_gpar)
  if (length(list) == 1)
    return(list[[1]])

  keys <- unique(unlist(lapply(list, names)))

  base <- list[[1]]
  base <- modifyList(base, get.gpar(setdiff(keys, names(base))))

  list.expanded <- lapply(list, function(x) modifyList(base, x))

  ret <- lapply(keys, function(k)
                unname(sapply(list.expanded, function(el) el[[k]])[index]))
  names(ret) <- keys
  class(ret) <- "gpar"
  ret
}

## This checks that an object is a gpar (or if is NULL creates an
## empty gpar because that's what grid implicitly assumes).  It's used
## in combine_gpars() above, but I'm not sure if it's generally useful.
check_gpar <- function(gp) {
  if (is.null(gp))
    gp <- gpar()
  else if (!inherits(gp, "gpar"))
    stop("Argument is not a gpar or NULL")
  if (length(gp) > 1 && any(sapply(gp, length) != 1))
    stop("All elements of 'gp' must be scalar")
  gp
}
