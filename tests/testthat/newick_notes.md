# The Newick format

There are a bunch of newick readers out there, and little
description of the format.

The closest thing to a "formal" description is the page here:
  http://evolution.genetics.washington.edu/phylip/newicktree.html

Neither ape's read.tree() nor phylip's treeread() will read
unifurcations, but phytools' read.nexus will.

* The tree ends with a semicolon.

* The bottommost node in this tree is an interior node, not a
  tip. Interior nodes are represented by a pair of matched
  parentheses. Between them are representations of the nodes that are
  immediately descended from that node, separated by commas.

* Tips are represented by their names. A name can be any string of
  printable characters except blanks, colons, semicolons, parentheses,
  and square brackets.

* An underscore character ("_") stands for a blank; any of these in a
  name will be converted to a blank when it is read in.

* Any name may also be empty: a tree like "(,(,,),);" is allowed.

* Trees can be multifurcating at any level.

* Branch lengths can be incorporated into a tree by putting a real
  number, with or without decimal point, after a node and preceded by
  a colon.  This represents the length of the branch immediately below
  that node. Thus the above tree might have lengths represented as:
  "(B:6.0,(A:5.0,C:3.0,E:4.0):5.0,D:11.0);"

# Test suite:

I would like to be able to parse a wide range of trees, as a test.
Could start with the Harmon trees, but do others too.  This could be
kept separate from the rest of the tests, and grabbing files from
dryad etc.

On my computer, I have a file with comments for testing:
nescent/ksi/proteaceae/

# Performance

The tree parser in the current tests is very slow; it takes about .4s
to read a 200 taxon tree, wheras ape takes 0.02s (so 20x slower).
Writing is even worse; ape takes 0.015s but we take 1.1s.

Here it is clear that it is the translation overhead killing us;
This will be a problem is we want really super fast access and a
few careful Rcpp::Function calls might be better than the big
heavyweight things that I currently have.

```
Rprof()
for (i in 1:5)
  tr <- read.newick(str)
Rprof(NULL)
summaryRprof()
```
