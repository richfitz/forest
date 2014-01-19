forest.from.ape <- function(phy) {
  if (!inherits(phy, "phylo"))
    stop("Need a 'phylo' object to convert")
  phy   <- reorder(phy, "pruningwise")
  n.tip <- length(phy$tip.label)
  from  <- phy$edge[,1]
  to    <- phy$edge[,2]
  n     <- phy$Nnode + n.tip

  node.label <- phy$node.label
  if (is.null(node.label))
    node.label <- rep.int("", phy$Nnode)
  label  <- c(phy$tip.label, node.label)

  length <- phy$edge.length[match(seq_len(n), phy$edge[,2])]
  if (is.null(length))
    length <- rep.int(NA_real_, n)

  order <- c(seq_len(n.tip), unique(from))

  from_ape_internal(order-1L, from-1L, to-1L, label, length)
}

## Going from forest->ape is hard because without unique node labels
## it's just not going to be possible to relate ape indices to nodes;
## we set up an ape node but have to refer to it again on another pass
## when we do the join (if written in pre/post order traversal).
## Recursively we can hold a node index on hand and work with that.
##
## I would rather a non-recursive algorithm, but given how this will
## be used I don't really see that it's worth the time right now
## (similarly, this would be faster in C, but I for this never to be
## an important part of a workflow).  According to Cstack_info(), we
## get through 5 expression levels per recursive call, so at the
## default level of options("expressions") we are good for a node
## depth of around 1,000.  That's either a modest tree (if fully
## asymmetric) or a huge tree (if fully balanced).  It will do for
## now.
ape.from.forest <- function(tr) {
  n.tip <- tr$tips
  n.node <- tr$nodes

  edge <- matrix(nrow=0, ncol=2)
  length <- numeric(0)
  label <- character(0)

  # Indices of the next tip and node we see:
  root <- n.tip + 1  # ape root is n.tip + 1
  i.tip <- 1         # haven't seen any tip yet.
  i.node <- root + 1 # seen the root already.

  rbind0 <- function(...) rbind(..., deparse.level=0)
  process.node <- function(index, first, last) {
    while (first$differs(last)) {
      subtr <- first$value # or just first$post_increment()
      nd <- subtr$begin()$value

      is.tip <- subtr$childless
      if (is.tip) {
        i <- i.tip
        i.tip <<- i.tip + 1
      } else {
        i <- i.node
        i.node <<- i.node + 1
      }

      edge   <<- rbind0(edge, c(index, i))
      length <<- c(length, nd$length)
      label  <<- c(label, nd$label)

      if (!is.tip)
        process.node(i, subtr$begin_sub_child(), subtr$end_sub_child())
      first$increment()
    }
  }

  process.node(root, tr$begin_sub_child(), tr$end_sub_child())

  tip.label <- label[match(seq_len(n.tip), edge[,2])]
  node.label <- c(tr$root()$label,
                  label[match((root+1):(n.tip+n.node), edge[,2])])

  storage.mode(edge) <- "integer"
  if (all(is.na(length)))
    length <- NULL
  if (all(node.label == ""))
    node.label <- NULL

  structure(list(edge=edge,
                 tip.label=tip.label,
                 edge.length=length,
                 Nnode=as.integer(n.node),
                 node.label=node.label),
            class="phylo",
            order="cladewise")
}
