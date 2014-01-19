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
## be used I don't really see that it's worth the time right now.
ape.from.forest <- function(tree) {
  obj <- to_ape_internal(tree)
  edge <- obj$edge
  label <- obj$label
  length <- obj$length
  n.tip <- tree$tips
  n.node <- tree$nodes

  tip.index  <- setdiff(edge[,2], edge[,1])
  node.index <- setdiff(seq_len(n.tip + n.node), tip.index)
  edge[] <- match(edge, c(tip.index, node.index))

  tip.label <- label[match(seq_len(n.tip), edge[,2])]
  i <- if (n.node > 1)
    match((n.tip+2):(n.tip+n.node), edge[,2]) else integer(0)
  node.label <- c(tree$root()$label, label[i])

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
