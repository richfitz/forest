#ifndef _FOREST_MODELS_RCPP_PRE_HPP_
#define _FOREST_MODELS_RCPP_PRE_HPP_

#include "models/calculator.hpp" // branch_pair
#include <RcppCommon.h>

namespace Rcpp {
template <typename T> SEXP wrap(const forest::models::branch_pair<T>& obj);
namespace traits {
template <typename T> class Exporter< forest::models::branch_pair<T> >;
}
}

#endif
