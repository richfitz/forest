#ifndef _FOREST_TREE_RCPP_POST_HPP_
#define _FOREST_TREE_RCPP_POST_HPP_

#include "tree/rcpp_pre.hpp"

#include <Rcpp.h>

namespace forest {
typedef forest::node<Rcpp::RObject> rnode;
typedef forest::tree_wrapped<rnode> rtree;
typedef rtree::subtree_wrapped_type rsubtree;

// This is all we're defining for now.  Will grow over time though.
// Really this one should only work for T_in = Rcpp::RObject and
// Rcpp::List.

// NOTE: This does not quite do what is wanted yet; I think that it is
// being over-eager in what is being converted.  In particular, I'd
// like to specialise this so that we only go down this route when
// T_in is Rcpp::RObject, but using the definition
//
//   template <typename T_out>
//   T_out data_convert(const Rcpp::RObject& obj) {
//     return Rcpp::as<T_out>(obj);
//   }
//
// gives a linker error.
//
// At the least it would be nice to specialise the T_out = T_in case
// to avoid a bunch of allocations.  This will have to do for now
// though (also, without careful tests I don't know when or if we use
// this without an Rcpp type as T_in).
template <typename T_out, typename T_in>
T_out data_convert(const T_in& obj) {
  return Rcpp::as<T_out>(Rcpp::wrap(obj));
}

}

RCPP_EXPOSED_CLASS_NODECL(forest::rnode)
RCPP_EXPOSED_CLASS_NODECL(forest::rtree)
RCPP_EXPOSED_CLASS_NODECL(forest::rsubtree)

// These are bit weird.  To export a tree (treetree::tree<T>) to R, we
// need to put it into a forest::tree_wrapped class; this arranges for
// that.  Then the wrapped classes still need to be wrappable (see for
// example the RCPP_EXPOSED_CLASS_NODECL(forest::rtree) above, but
// also in the models code).
namespace Rcpp {
template <typename T>
SEXP wrap(const treetree::tree<T>& obj) {
  return Rcpp::wrap(forest::tree_wrapped<T>(obj));
}
template <typename T>
SEXP wrap(const treetree::subtree<T>& obj) {
  return Rcpp::wrap(forest::subtree_wrapped<T>(obj));
}

namespace traits {
template <typename T>
class Exporter< treetree::tree<T> > {
public:
  Exporter (SEXP x) : t(Rcpp::as<forest::tree_wrapped<T> >(x).tree_) {}
  inline treetree::tree<T> get() { return t; }
private:
  treetree::tree<T> t;
};

template <typename T>
class Exporter< treetree::subtree<T> > {
public:
  Exporter (SEXP x) : t(Rcpp::as<forest::subtree_wrapped<T> >(x).subtree_) {}
  inline treetree::subtree<T> get() { return t; }
private:
  treetree::subtree<T> t;
};

}
}

#endif
