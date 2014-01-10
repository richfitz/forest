PAREN_OPEN  <- 1L
PAREN_CLOSE <- 2L
COMMA       <- 3L
SEMICOLON   <- 4L
NODE        <- 5L

## First, ignore comments so all parens and commas are taken as
## meaninful.  If we do that then the string can be pulled apart
## immediately with a regular expression.
##
## This will be harder if comments are embedded as we'll need to go
## through and check that the comments do not contain embedded
## parentheses or commas.  The simplest thing to do there would be to
## rip all the comments out.
tokenise.newick <- function(str) {
  if (length(str) > 1)
    str <- paste(str, collapse="")
  str <- sub("^[[:space:]]+", "", str, perl=TRUE)
  tokens <- strsplit(str, "(?=[(),;])", perl=TRUE)[[1]]
  tokens <- sub("[[:space:]]+$", "", tokens, perl=TRUE)

  type <- rep.int(NODE, length(tokens))
  type[tokens == "("] <- PAREN_OPEN
  type[tokens == ")"] <- PAREN_CLOSE
  type[tokens == ","] <- COMMA
  type[tokens == ";"] <- SEMICOLON

  ## Easier to assert about the semicolon here than elsewhere:
  i <- which(type == SEMICOLON)
  if (length(i) != 1L)
    stop("Expected exactly one semicolon")
  if (i != length(type))
    stop("Expected semicolon as last token")

  ## Then build our little iterator-type thing.
  check.nonempty <- function()
    if (length(type) == 0L)
      stop("Parse error -- reached end of tokens")
  peek <- function() {
    check.nonempty()
    type[[1]]
  }
  pop  <- function() {
    check.nonempty()
    ret <- tokens[[1]]
    type   <<- type[-1]
    tokens <<- tokens[-1]
    ret
  }
  list(peek=peek, pop=pop)
}

parse.newick <- function(token.stream) {
  parse.newick.node <- function(x) {
    x <- strsplit(x, ":", fixed=TRUE)[[1]]
    label <- x[[1]]
    length <- if (length(x) == 1) NA_real_ else as.numeric(x[[2]])
    new(xnode, NA_integer_, label, length)
  }

  if (token.stream$peek() != PAREN_OPEN)
    stop("Parse error -- expected opening parenthesis")
  token.stream$pop()

  ## Start building the contents of the tree:
  children <- list()
  while (token.stream$peek() != PAREN_CLOSE) {
    next.type <- token.stream$peek()
    if (next.type == PAREN_OPEN) {
      children <- c(children, parse.newick(token.stream))
    } else if (next.type == NODE) {
      children <- c(children,
                    parse.newick.node(token.stream$pop()))
    } else { # only COMMA left here
      token.stream$pop()
    }
  }
  token.stream$pop()

  if (token.stream$peek() == NODE)
    node <- parse.newick.node(token.stream$pop())
  else
    node <- new(xnode, NA_integer_)

  tr <- new(xtree, node)
  for (x in rev(children)) {
    # Surprisingly, the is_node_type() check failed here, even though
    # it worked on tree_of()
    if (inherits(x, "Rcpp_xnode"))
      tr$prepend(x)
    else
      tr$prepend_subtree(x)
  }
  tr
}

## And here is the interface function.
read.newick <- function(str) {
  parse.newick(tokenise.newick(str))
}

## The inverse: Newick string from a tree.
write.newick <- function(tr, digits=10) {
  fmt.digits <- paste0("%.", digits, "g")
  fmt <- function(nd) {
    len <- nd$length
    if (is.na(len))
      nd$label
    else
      paste(nd$label, sprintf(fmt.digits, len), sep=":")
  }

  f <- function(tr) {
    if (tr$childless) {
      fmt(tr$root())
    } else {
      str <- "("
      i1 <- tr$begin_sub_child()
      i2 <- tr$end_sub_child()
      while (i1$differs(i2)) {
        str <- c(str, f(i1$post_increment()$value))
        if (i1$differs(i2))
          str <- c(str, ",")
      }
      str <- c(str, ")", fmt(tr$root()))
      paste(str, collapse="")
    }
  }
  paste0(f(tr), ";")
}
