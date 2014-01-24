#ifndef _FOREST_RCPP_HPP_
#define _FOREST_RCPP_HPP_

#include "node.hpp"

#include <RcppCommon.h>

// Start with wrap, because it's much easier than as.
namespace Rcpp {
template <typename T> SEXP wrap(const forest::node<T>& obj);
namespace traits {
template <typename T> class Exporter< forest::node<T> >;
}
}

#include <Rcpp.h>

RCPP_EXPOSED_CLASS_NODECL(forest::node_wrapped<Rcpp::RObject>)

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
}
}

#endif
