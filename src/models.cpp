#include "models.hpp"
#include <Rcpp.h>

namespace Rcpp {
template<> SEXP wrap(const forest::models::gaussian& obj);
template<> SEXP wrap(const forest::models::gaussian& obj) {
  return NumericVector::create(_["mean"]      = obj.mean,
                               _["variance"]  = obj.variance,
                               _["log_scale"] = obj.log_scale);
}
template<> forest::models::gaussian as(SEXP obj);
template<> forest::models::gaussian as(SEXP obj) {
  std::vector<double> tmp = as< std::vector<double> >(obj);
  if (tmp.size() != 3)
    Rcpp::stop("Expected exactly three elements");
  return forest::models::gaussian(tmp[0], tmp[1], tmp[2]);
}
}

using forest::models::gaussian;
std::vector<double> test_gaussian(gaussian obj);
std::vector<double> test_gaussian(gaussian obj) {
  std::vector<double> ret;
  ret.push_back(obj.mean);
  ret.push_back(obj.variance);
  ret.push_back(obj.log_scale);
  return ret;
}

gaussian test_gaussian_product(gaussian x, gaussian y);
gaussian test_gaussian_product(gaussian x, gaussian y) {
  return x * y;
}

#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(models) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<forest::models::brownian_motion>("brownian_motion")
    .constructor()
    .property("parameters",
              &forest::models::brownian_motion::parameters,
              &forest::models::brownian_motion::set_parameters)
    .method("forward",
            &forest::models::brownian_motion::forward)
    .method("backward",
            &forest::models::brownian_motion::backward)
    .method("combine",
            &forest::models::brownian_motion::combine)
    ;
  Rcpp::function("test_gaussian", &test_gaussian);
  Rcpp::function("test_gaussian_product", &test_gaussian_product);
}
