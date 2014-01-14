#ifndef _FOREST_MISC_HPP_
#define _FOREST_MISC_HPP_

#include "treetree.hpp"

namespace forest {

// This generic method function can be overridden for more interesting
// types;  see node.hpp for an example.
template <typename T>
std::string node_label(const T& nd) {
  return boost::lexical_cast<std::string>(nd);
}

template <typename T>
std::vector<std::string> labels(const treetree::tree<T>& tr,
                                bool tip) {
  std::vector<std::string> ret;
  for (typename treetree::tree<T>::const_sub_pre_iterator
         it = tr.begin(); it != tr.end(); ++it) {
    if (it->childless() == tip)
      ret.push_back(node_label(it->root()));
  }
  return ret;
}

}

#endif
