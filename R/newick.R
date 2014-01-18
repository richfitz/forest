from.newick.string <- function(str)
  from_newick_string(split.newick.string(str))
to.newick.string <- function(tr, digits=10)
  to_newick_string(tr, digits)

split.newick.string <- function(str) {
  if (length(str) > 1)
    str <- paste(str, collapse="")
  str <- sub("^[[:space:]]+", "", str, perl=TRUE)
  tokens <- strsplit(str, "(?=[(),;])", perl=TRUE)[[1]]
  sub("[[:space:]]+$", "", tokens, perl=TRUE)
}
