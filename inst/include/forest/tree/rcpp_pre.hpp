#ifndef _FOREST_TREE_RCPP_PRE_HPP_
#define _FOREST_TREE_RCPP_PRE_HPP_

#include <forest/tree/node.hpp>
#include <forest/treetree.hpp>

#include <RcppCommon.h>

namespace Rcpp {
template <typename T> SEXP wrap(const treetree::tree<T>& obj);
template <typename T> SEXP wrap(const treetree::subtree<T>& obj);

namespace traits {
template <typename T> class Exporter< treetree::tree<T> >;
template <typename T> class Exporter< treetree::subtree<T> >;
}
}

#endif
