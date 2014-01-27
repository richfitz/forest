#ifndef _FOREST_RCPP_HPP_
#define _FOREST_RCPP_HPP_

#include "node.hpp"

#include <RcppCommon.h>

namespace Rcpp {
template <typename T> SEXP wrap(const forest::node<T>& obj);
template <typename T> SEXP wrap(const treetree::tree<T>& obj);
template <typename T> SEXP wrap(const treetree::subtree<T>& obj);

namespace traits {
template <typename T> class Exporter< forest::node<T> >;
template <typename T> class Exporter< treetree::tree<T> >;
template <typename T> class Exporter< treetree::subtree<T> >;
}
}

#include <Rcpp.h>

// TODO: push into forest namespace
typedef forest::node_wrapped<Rcpp::RObject>    rnode;
typedef forest::tree_wrapped<rnode::node_type> rtree;
typedef rtree::subtree_wrapped_type            rsubtree;

RCPP_EXPOSED_CLASS_NODECL(rnode)
RCPP_EXPOSED_CLASS_NODECL(rtree)
RCPP_EXPOSED_CLASS_NODECL(rsubtree)

// Convert to our node_wrapper with the data slot being an RObject --
// this is possible because we declared this type to be exportable
// (above), enabling as and wrap on this data type.  Then, take that
// data slot and convert it to type T, constructing a new node with
// this.
namespace {
template <typename T>
forest::node<T> helper_node_wrapped(SEXP obj) {
  forest::node<Rcpp::RObject> nd =
    Rcpp::as<forest::node_wrapped<Rcpp::RObject> >(obj).node_;
  return forest::node<T>(nd.label_, nd.length_, Rcpp::as<T>(nd.data_));
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
// constructing.
template<>
SEXP wrap(const forest::node<Rcpp::RObject>& obj) {
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
