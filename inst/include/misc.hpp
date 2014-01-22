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

// Are *all* branch lengths in the tree valid?
template <typename T>
bool has_branch_lengths(const treetree::tree<T>& tr) {
  for (typename treetree::tree<T>::const_pre_iterator
         it = tr.begin(); it != tr.end(); ++it)
    if (ISNA(it->length_) && it != tr.begin())
      return false;
  return true;
}

// Height above the root node; by definition the root is taken to
// have height 0.  In the max_h test, we could restrict this to
// cases where `it->childless()` is true, but the current approach
// allows negative branch lengths so is more general.
//
// The depths are updated so that the most recent tip has depth 0.0.
// In an ultrametric tree, all tips will have depth 0.0 (within
// rounding error).
template <typename T>
void update_heights(treetree::tree<T>& tr) {
  typedef typename treetree::tree<T>::pre_iterator pre_iterator;
  if (!has_branch_lengths(tr))
    Rcpp::stop("Tree does not have complete branch lengths");
  double max_h = 0.0;
  for (pre_iterator it = tr.begin(); it != tr.end(); ++it) {
    const double h = it == tr.begin() ? 0.0 :
      it->length_ + treetree::parent(it)->height_;
    it->height_ = h;
    if (h > max_h)
      max_h = h;
  }
  // Set the depth as distance below highest tip:
  for (pre_iterator it = tr.begin(); it != tr.end(); ++it)
    it->depth_ = max_h - it->height_;
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

template <typename T>
std::vector<double> depths(const treetree::const_subtree<T>& tr) {
  std::vector<double> ret;
  for (typename treetree::const_subtree<T>::const_pre_iterator
         it = tr.begin(); it != tr.end(); ++it)
    ret.push_back(it->depth_);
  return ret;
}
template <typename T>
std::vector<double> depths(const treetree::subtree<T>& tr) {
  return depths(treetree::const_subtree<T>(tr));
}
template <typename T>
std::vector<double> depths(const treetree::tree<T>& tr) {
  return depths(treetree::const_subtree<T>(tr));
}

// NOTE: We've already swept through the tree and computed the
// "depth"; by definition the most recent tip has depth 0.0 and every
// other tip has a depth larger than that.  So all we need to do is
// find the largest tip.
//
// NOTE: You could make the case that the empty tree (size 0), the
// tree with just a root (size 1) and just one branch (size 2) are
// poorly defined with ultrametricness.  I've not thrown an error
// here, but perhaps should.
template <typename T>
bool is_ultrametric(treetree::tree<T> tr, double eps) {
  update_heights(tr);

  // TODO: sub_leaf_adapter might be better here?
  for (typename treetree::tree<T>::const_sub_pre_iterator
         it = tr.begin_sub(); it != tr.end_sub(); ++it)
    if (it->childless() && it->begin()->depth_ > eps)
        return false;
  return true;
}

// NOTE: I'm not certain how to deal with the empty tree or a tree
// with no leaves; they're not *not* binary, but they're not
// *definitely* binary.  Could return NA_LOGICAL here in those cases,
// but that does not play nicely with bool as a return type so I'll
// just throw instead.
template <typename T>
bool is_binary(const treetree::tree<T>& tr) {
  if (tr.size() < 2)
    Rcpp::stop("Tree of size < 2 does not have defined binaryness");
  for (typename treetree::tree<T>::const_sub_pre_iterator
         it = tr.begin_sub(); it != tr.end_sub(); ++it) {
    if (!it->childless()) {   // not terminal
      if (it->arity() != 2) { // not binary
        return false;
      }
    }
  }
  return true;
}

template <typename T>
struct label_finder {
  label_finder(const std::string& label) : target(label) {}
  bool operator()(const T& nd) const {
    return nd.has_label() && nd.label_ == target;
  }
  bool operator()(const treetree::subtree<T>& sub) const {
    const T& nd = sub.root();
    return nd.has_label() && nd.label_ == target;
  }
  const std::string target;
};

template <typename T, typename Iterator>
Iterator locate_node_by_label(Iterator first, Iterator last,
                              const std::string& label) {
  return std::find_if(first, last, label_finder<T>(label));
}

template <typename T, typename Iterator>
bool is_terminal(Iterator it) {
  typename treetree::tree<T>::sub_pre_iterator sub = it;
  return sub->childless();
}

// Same as above, but make sure we do find it.
template <typename T, typename Iterator>
Iterator locate_tip_by_label(Iterator first, Iterator last,
                             const std::string& label) {
  Iterator ret = locate_node_by_label<T>(first, last, label);
  if (ret == last)
    Rcpp::stop("Did not find tip " + label + " in tree\n");
  if (!is_terminal<T>(ret))
    Rcpp::stop("The label " + label + " is not terminal\n");
  return ret;
}

// TODO: Repetition with above; combine with flags perhaps?
template <typename T, typename Iterator>
Iterator locate_internal_by_label(Iterator first, Iterator last,
                                  const std::string& label) {
  Iterator ret = locate_node_by_label<T>(first, last, label);
  if (ret == last)
    Rcpp::stop("Did not find node " + label + " in tree\n");
  if (is_terminal<T>(ret))
    Rcpp::stop("The label " + label + " is not internal\n");
  return ret;
}

// NOTE: Currently, will refuse to return a subtree of a terminal
// node; not sure if that is the best behaviour or not, actually.
// Could be useful in some contexts.
template <typename T>
treetree::subtree<T> subtree_at_label(treetree::subtree<T>& tr,
                                      const std::string& label) {
  return *locate_internal_by_label<T>(tr.begin_sub(), tr.end_sub(),
                                      label);
}

template <typename T>
treetree::subtree<T> subtree_at_label(treetree::tree<T>& tr,
                                      const std::string& label) {
  return *locate_internal_by_label<T>(tr.begin_sub(), tr.end_sub(),
                                      label);
}

// Check that all tip and/or node labels are present in the vector
// 'names'.  Note that this does not check the reverse (that all
// elements in 'names' are present in the tree).
//
// I'm also not checking that names is unique, or that names in the
// tree are unique.  Both of these are probably required for more
// sensible use of this sort of thing.
//
// This uses a completely naive search.  If we sort names, then we
// could use std::binary_search.  But using boost::unordered_set or
// std::tr1::unordered_set would give O(1) lookup, which will be way
// faster if this turns out to be annoying.  But the interface won't
// change.
template <typename T>
bool check_names(const treetree::tree<T>& tr,
                 const std::vector<std::string>& names,
                 bool tip, bool node) {
  for (typename treetree::tree<T>::const_sub_pre_iterator
         it = tr.begin(); it != tr.end(); ++it)
    if ((tip  && it->childless()) ||
        (node && !it->childless()))
      if (std::find(names.begin(), names.end(),
                    node_label(it->root())) == names.end())
        return false;
  return true;
}

}

#endif
