#ifndef _FOREST_MODELS_RCPP_POST_HPP_
#define _FOREST_MODELS_RCPP_POST_HPP_

#include "models/common.hpp"
#include "models/rcpp_pre.hpp"

#include <Rcpp.h>

RCPP_EXPOSED_CLASS_NODECL(forest::models::gaussian)
RCPP_EXPOSED_CLASS_NODECL(forest::models::brownian_motion)

namespace {
template <typename T>
forest::models::branch_pair<T> helper_branch_pair(SEXP obj) {
  Rcpp::List obj_l = Rcpp::as<Rcpp::List>(obj);
  if (obj_l.size() != 2)
    Rcpp::stop("Conversion failure");
  // TODO: Select on names rootward/tipward instead of position 0/1?
  return forest::models::branch_pair<T>(Rcpp::as<T>(obj_l[0]),
                                        Rcpp::as<T>(obj_l[1]));
}
}

namespace Rcpp {
template <typename T>
SEXP wrap(const forest::models::branch_pair<T>& obj) {
  return List::create(_["rootward"] = Rcpp::wrap(obj.rootward),
                      _["tipward"]  = Rcpp::wrap(obj.tipward));
}
namespace traits {
template <typename T>
class Exporter< forest::models::branch_pair<T> > {
public:
  Exporter(SEXP x) : t(helper_branch_pair<T>(x)) {}
  inline forest::models::branch_pair<T> get() { return t; }
private:
  forest::models::branch_pair<T> t;
};
}
}

#endif
