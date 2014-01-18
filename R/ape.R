forest.from.ape <- function(phy) {
  if (!inherits(phy, "phylo"))
    stop("Need a 'phylo' object to convert")
  phy   <- reorder(phy, "pruningwise")
  from  <- phy$edge[,1]
  to    <- phy$edge[,2]
  n     <- phy$Nnode + Ntip(phy)

  node.label <- phy$node.label
  if (is.null(node.label))
    node.label <- rep.int("", phy$Nnode)
  label  <- c(phy$tip.label, node.label)

  length <- phy$edge.length[match(seq_len(n), phy$edge[,2])]
  if (is.null(length))
    length <- rep.int(NA_real_, n)

  order <- c(seq_len(Ntip(phy)), unique(from))

  from_ape_internal(order-1L, from-1L, to-1L, label, length)
}

# This is obviously poor form, but gives something to go on for now.
ape.from.forest <- function(tree) {
  read.tree(text=to.newick.string(tree))
}
