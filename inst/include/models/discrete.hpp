#ifndef _FOREST_MODELS_DISCRETE_HPP_
#define _FOREST_MODELS_DISCRETE_HPP_

// For the first instance hard code this for two array<double,2>?
// #include <boost/array.hpp>
//
// I'd really like to be able to get some sort of run time static
// sizing.  Removing the ability to resize would get me there though.
// That would require the duplicate_topology type code to allow a
// "default" value so that we don't default construct the data (which
// leads to a zero-length case).  Ignore this issue for now though.
// Can probably uncover all the places that this is needed by
// disabling the default constructor.

#include <R.h>               // NA_REAL
#include <vector>            // std::vector
#include <boost/utility.hpp> // boost:prior
#include "../util.hpp"       // stop

namespace forest {
namespace models {

struct discrete {
  discrete() : log_scale(NA_REAL) {}
  discrete(const std::vector<double>& probabilities_, double log_scale_)
    : probabilities(probabilities_), log_scale(log_scale_) {}

  static discrete from_R(SEXP obj) {
    std::vector<double> pars = Rcpp::as<std::vector<double> >(obj);
    if (pars.size() < 1)
      stop("Need at least one elements");
    std::vector<double> p(pars.begin(), boost::prior(pars.end()));
    return discrete(p, pars.back());
  }

  // This also makes me think that a reference or iterator-based
  // approach would be better as this is going to involve a lot of
  // vector creation.  But avoid doing this until I know that is the
  // case.
  discrete operator*(const discrete& rhs) const {
    util::check_length(rhs.size(), size());
    discrete ret;
    ret.probabilities.reserve(size());
    for (size_t i = 0; i < probabilities.size(); ++i)
      ret.probabilities.push_back(probabilities[i] * rhs.probabilities[i]);
    ret.log_scale = log_scale + rhs.log_scale;
    return ret;
  }

  // Invalid if any element is negative or if no element is positive.
  bool valid() const {
    bool ok = false;
    for (std::vector<double>::const_iterator it = probabilities.begin();
         it != probabilities.end(); ++it) {
      if (*it < 0)
        return false;
      ok = ok || *it > 0;
    }
    return ok;
  }

  double& operator[](size_t i) {return probabilities[i];}
  void resize(size_t n) {probabilities.resize(n, NA_REAL);}
  size_t size() const {return probabilities.size();}

  double r_at(size_t idx) {
    return operator[](util::check_bounds_r(idx, size()));
  }

  std::vector<double> probabilities;
  double log_scale;
};

}
}

#endif
