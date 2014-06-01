#ifndef _FOREST_TREE_TREE_HPP_
#define _FOREST_TREE_TREE_HPP_

#include <Rcpp.h>

// There is a lot to implement here.  We need
//
// * Constructors (empty, root node)
// * Copy (or something to make that work in R)
// * clear
// * Basic interrogation(empty, size, arity, childless, representation)
// * Accessors: (root_node, , at, insert_at)

// Then all the stuff that I wrote:

// tips, nodes, tip_labels, node_labels, heights, depths, is_binary,
// has_branch_lengths, is_ultrametric, update_heights,
// collapse_singles, drop_tips, rotate, ladderise, get_subtree,
// check_names, associate_data, copy_structure.
//
// However, while going back and adding these things in I need to be
// very careful to think about which bits are better as free
// functions and which are better for methods.
//
// And I need to do this for subtrees too.  And for subtree also
// to_tree.
//
// This file needs renaming and/or merging into the other files in
// this directory.

namespace forest {

// This duplication here is not very nice, but it can be avoided if
// the as/wrap functions can organise a const_subtree for us.
template <typename T>
std::string representation(const T& tr) {
  return boost::lexical_cast<std::string>(tr);
}

}

#endif
