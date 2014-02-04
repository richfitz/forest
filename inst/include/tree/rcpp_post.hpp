#ifndef _FOREST_TREE_RCPP_POST_HPP_
#define _FOREST_TREE_RCPP_POST_HPP_

#include "tree/rcpp_pre.hpp"

#include <Rcpp.h>

namespace forest {
typedef forest::node_wrapped<Rcpp::RObject>    rnode;
typedef forest::tree_wrapped<rnode::node_type> rtree;
typedef rtree::subtree_wrapped_type            rsubtree;

// This is all we're defining for now.  Will grow over time though.
// Really this one should only work for T_in = Rcpp::RObject and
// Rcpp::List.  It might be that some need to go through
//   Rcpp::as<T_out>(Rcpp::wrap(obj))
// to work though.  That could be too much though, so only allowing
// things that will implicitly convert to SEXP at the moment.

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
  return Rcpp::as<T_out>(obj);
}

}

RCPP_EXPOSED_CLASS_NODECL(forest::rnode)
RCPP_EXPOSED_CLASS_NODECL(forest::rtree)
RCPP_EXPOSED_CLASS_NODECL(forest::rsubtree)

// Convert to our node_wrapper with the data slot being an RObject --
// this is possible because we declared this type to be exportable
// (above), enabling as and wrap on this data type.  Then, take that
// data slot and convert it to type T, constructing a new node with
// this.
//
// TODO: Probably need a RObject-specialised version of this...
namespace {
template <typename T>
forest::node<T> helper_node_wrapped(SEXP obj) {
  return Rcpp::as<forest::node_wrapped<Rcpp::RObject> >(obj).node_.copy<T>();
}
}

namespace Rcpp {
template <typename T>
SEXP wrap(const forest::node<T>& obj) {
  return wrap(forest::node_wrapped<RObject>(obj.label_, obj.length_,
                                            wrap(obj.data_)));
}
template <typename T>
SEXP wrap(const treetree::tree<T>& obj) {
  return Rcpp::wrap(forest::tree_wrapped<T>(obj));
}
template <typename T>
SEXP wrap(const treetree::subtree<T>& obj) {
  return Rcpp::wrap(forest::subtree_wrapped<T>(obj));
}

// This might be a micro-optimisation, but it does work; if we want a
// node<RObject> we can just take the node from within the wrapper.
// That will use the copy constructor rather than manually
// constructing.  The inline is required to avoid multiple-definition
// problems.
template<>
inline SEXP wrap(const forest::node<Rcpp::RObject>& obj) {
  return wrap(forest::node_wrapped<RObject>(obj));
}

namespace traits {
template <typename T>
class Exporter< forest::node<T> > {
public:
  Exporter(SEXP x) : t(helper_node_wrapped<T>(x)) {}
  inline forest::node<T> get() { return t; }
private:
  forest::node<T> t;
};

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
