#ifndef _FOREST_MISC_HPP_
#define _FOREST_MISC_HPP_

// Miscellaneous things to do with treetree trees (*not* the wrapper
// in src/tree.hpp).

#include "treetree.hpp"

namespace forest {

// * Count the number of tips in a tree or subtree:
template <typename T>
size_t count_tips(const treetree::const_subtree<T>& tr) {
  return static_cast<size_t>
    (std::count_if(tr.begin_sub(), tr.end_sub(),
                   &treetree::childless< treetree::const_subtree<T> >));
}
template <typename T>
size_t count_tips(const treetree::subtree<T>& tr) {
  return count_tips(treetree::const_subtree<T>(tr));
}
template <typename T>
size_t count_tips(const treetree::tree<T>& tr) {
  return count_tips(treetree::const_subtree<T>(tr));
}

// * Count the number of (internal) nodes in a tree or subtree:
//
// The typing here is a bit of a copout, but does seem to work:
template <typename T>
size_t count_nodes(const T& tr) {
  return tr.size() - count_tips(tr);
}

// * Extract node or tip labels.  In node.hpp we define a more useful
// template for extracting names, but every node can be converted to
// string with boost::lexical_cast, so have something valid here at
// least.
template <typename T>
std::string node_label(const T& nd) {
  return boost::lexical_cast<std::string>(nd);
}

template <typename T>
std::vector<std::string> labels(const treetree::const_subtree<T>& tr,
                                bool tip) {
  std::vector<std::string> ret;
  for (typename treetree::const_subtree<T>::const_sub_pre_iterator
         it = tr.begin(); it != tr.end(); ++it) {
    if (it->childless() == tip)
      ret.push_back(node_label(it->root()));
  }
  return ret;
}
template <typename T>
std::vector<std::string> labels(const treetree::subtree<T>& tr, bool tip) {
  return labels(treetree::const_subtree<T>(tr), tip);
}
template <typename T>
std::vector<std::string> labels(const treetree::tree<T>& tr, bool tip) {
  return labels(treetree::const_subtree<T>(tr), tip);
}

// Extract tree heights.  These are set by update_heights()
template <typename T>
std::vector<double> heights(const treetree::const_subtree<T>& tr) {
  std::vector<double> ret;
  for (typename treetree::const_subtree<T>::const_pre_iterator
         it = tr.begin(); it != tr.end(); ++it)
    ret.push_back(it->height_);
  return ret;
}
template <typename T>
std::vector<double> heights(const treetree::subtree<T>& tr) {
  return heights(treetree::const_subtree<T>(tr));
}
template <typename T>
std::vector<double> heights(const treetree::tree<T>& tr) {
  return heights(treetree::const_subtree<T>(tr));
}



}

#endif
