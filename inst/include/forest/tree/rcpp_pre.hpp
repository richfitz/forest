#ifndef _FOREST_TREE_RCPP_PRE_HPP_
#define _FOREST_TREE_RCPP_PRE_HPP_

#include <forest/tree/node.hpp>
#include <forest/treetree.hpp>
class index;

#include <RcppCommon.h>

namespace Rcpp {
template <> SEXP wrap(const forest::util::index& obj);

template <typename T> SEXP wrap(const forest::node<T>& obj);

template <typename T> SEXP wrap(const treetree::tree<T>& obj);
template <typename T> SEXP wrap(const treetree::subtree<T>& obj);

template <> forest::util::index as(SEXP);

namespace traits {
template <typename T> class Exporter< forest::node<T> >;

template <typename T> class Exporter< treetree::tree<T> >;
template <typename T> class Exporter< treetree::subtree<T> >;
}
}

#endif
