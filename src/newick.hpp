#ifndef _FOREST_NEWICK_HPP_
#define _FOREST_NEWICK_HPP_

#include <ostream>
#include <iomanip>

#include "tree.hpp"
#include "node.hpp"

namespace forest {

template <typename T>
void to_newick(std::ostream& out, const node<T>& nd) {
  out << nd.label_;
  if (nd.has_length())
    out << ":" << nd.length_;
}

template <typename T>
void to_newick(std::ostream& out, treetree::const_subtree<T> tr) {
  if (!tr.childless()) {
    out << "(";
    for (typename treetree::const_subtree<T>::const_sub_child_iterator
           it=tr.begin_sub_child(); it != tr.end_sub_child(); ++it) {
      to_newick(out, *it);
      out << (boost::next(it) == tr.end_sub_child() ? ")" : ",");
    }
  }
  to_newick(out, tr.root());
}

template <typename T>
void to_newick(std::ostream& out, treetree::subtree<T> tr) {
  to_newick(out, treetree::const_subtree<T>(tr));
}
template <typename T>
void to_newick(std::ostream& out,const treetree::tree<T>& tr) {
  if (!tr.empty())
    to_newick(out, treetree::const_subtree<T>(tr));
}

// NOTE:
//
// I can either declare/export this way:
//   template <typename T> std::string f(const T& tr)
//   Rcpp::function("f", &forest::f<xtree>);
//
// Or this way:
//
//   template <typename T> std::string f(const treetree::tree<T>& tr)
//   Rcpp::function("f", &forest::f<xtree::value_type>);
template <typename T>
std::string to_newick_string(treetree::tree<T> tr, int digits) {
  std::stringstream out;
  out << std::setprecision(digits);
  to_newick(out, tr);
  out << ";";
  return out.str();
}

}

#endif
