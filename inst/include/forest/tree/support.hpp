#ifndef _FOREST_TREE_SUPPORT_HPP_
#define _FOREST_TREE_SUPPORT_HPP_

#include <forest/tree/node.hpp>
#include <forest/treetree.hpp>
#include <forest/util.hpp> // util::stop

namespace forest {

template <typename T>
struct label_finder {
  label_finder(const std::string& label) : target(label) {}
  bool operator()(const T& nd) const {
    return nd.has_label() && nd.label_ == target;
  }
  bool operator()(const treetree::subtree<T>& sub) const {
    // should be
    //   return (*this)(sub.root());
    // to avoid repetition I think.
    const T& nd = sub.root();
    return nd.has_label() && nd.label_ == target;
  }
  const std::string target;
};

template <typename T, typename Iterator>
Iterator locate_node_by_label(Iterator first, Iterator last,
                              const std::string& label) {
  Iterator ret = std::find_if(first, last, label_finder<T>(label));
  if (ret == last) {
    util::stop("Did not find node " + label + " in tree\n");
  }
  return ret;
}

}

#endif
