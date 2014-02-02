#ifndef _FOREST_MODELS_GAUSSIAN_HPP_
#define _FOREST_MODELS_GAUSSIAN_HPP_

#include <R.h>      // M_PI, NA_REAL
#include <vector>   // std::vector
#include "../util.hpp" // stop

namespace forest {
namespace models {

// NOTE: The 'from_R' method could be set up as a constructor -- replace
//   'static gaussian from_R'
// with
//   'gaussian'
// thoug that will enable implcit type conversions for as/wrap, and
// may not play nicely with RCPP_EXPOSED_CLASS in
// models/rcpp_post.hpp
//
// It potentially also interferes with using different constructors
// from R.

struct gaussian {
  gaussian()
    : mean(NA_REAL), variance(NA_REAL), log_scale(NA_REAL) {}
  gaussian(double mean_, double variance_, double log_scale_)
    : mean(mean_), variance(variance_), log_scale(log_scale_) {}
  // Useful from R:
  gaussian(const std::vector<double>& pars)
    : mean(pars.at(0)), variance(pars.at(1)), log_scale(pars.at(2)) {
    if (pars.size() != 3) // NOTE: Only > 3 will throw here
      stop("Expected exactly three parameters");
  }
  // This implicitly sets variance = 0 and log_scale = 0 if not
  // given.
  static gaussian from_R(SEXP obj) {
    std::vector<double> p = Rcpp::as<std::vector<double> >(obj);
    if (p.size() == 0)
      stop("Missing a mean");
    else if (p.size() < 3)
      p.resize(3, 0.0); // variance, log_scale
    return gaussian(p[0], p[1], p[2]);
  }
  // Also add convolve here?
  gaussian operator*(const gaussian& rhs) const {
    const double vv = variance + rhs.variance;
    const double dx = mean - rhs.mean;
    return
      gaussian((mean * rhs.variance + rhs.mean * variance) / vv,
               variance * rhs.variance / vv,
               log_scale + rhs.log_scale -
               dx * dx / (2 * vv) - log(2 * M_PI * vv) / 2);
  }
  bool valid() const {
    return variance >= 0;
  }
  double mean;
  double variance;
  double log_scale;
};

}
}

#endif
