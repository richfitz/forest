#ifndef _FOREST_INTERROGATION_HPP_
#define _FOREST_INTERROGATION_HPP_

// Ask the tree things
namespace forest {

// NOTE: I'm not certain how to deal with the empty tree or a tree
// with no leaves; they're not *not* binary, but they're not
// *definitely* binary.  Could return NA_LOGICAL here in those cases,
// but that does not play nicely with bool as a return type so I'll
// just throw instead.
template <typename T>
bool is_binary_tree(const treetree::tree<T>& tr) {
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

}

#endif
