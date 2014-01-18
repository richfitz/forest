#ifndef _FOREST_APE_HPP_
#define _FOREST_APE_HPP_

#include "tree.hpp"

namespace forest {

// Build a tree out of an ape phylo object:
//
// NOTE: this is templated assuming that we are making a tree of
// forest::node's, not of general data.  However, we can probably
// generalise this later if it seems useful by putting a "data" vector
// (Rcpp::List, or SEXP, perhaps) with the required data (in the
// forest::node case it will be branch lengths, labels and ape
// indices, but in other cases it might be just the indices).  Given
// how this will be used though, I'm comfortable with this level of
// specification.
//
// Assumed, but not checked:
//
// * order, from and to are in base 0 and no number exceeds n-1, and
//   order is a valid post order traversal.
// * length and label are length n
// * from and to are length n-1, because there is no root edge.
//
// Because we use splice, this should not require any more memory than
// needed to hold the entire tree.  Hopefully the number of copies is
// not too bad.
template <typename T>
treetree::tree<T>
from_ape_internal(const std::vector<size_t>& order,
                  const std::vector<size_t>& from,
                  const std::vector<size_t>& to,
                  const std::vector<std::string>& label,
                  const std::vector<double>& length) {
  typedef typename treetree::tree<T>     tree_type;
  typedef typename tree_type::value_type node_type;

  // First go through and work out who is descended from whom.
  std::vector< std::vector<size_t> > desc(order.size());
  for (size_t i = 0; i < from.size(); ++i)
    desc[from[i]].push_back(to[i]);

  std::vector<tree_type> sub(order.size());
  for (std::vector<size_t>::const_iterator i = order.begin();
       i != order.end(); ++i) {
    const size_t j = *i;
    tree_type sub_j(node_type(label[j], length[j],
                              Rcpp::RObject(Rcpp::wrap(j+1))));
    for (std::vector<size_t>::const_iterator
           x = desc[j].begin(); x != desc[j].end(); ++x) {
      sub_j.splice(sub_j.end_child(), sub[*x]);
    }
    sub[j] = sub_j;
  }

  return sub[order.back()];
}

}

#endif
