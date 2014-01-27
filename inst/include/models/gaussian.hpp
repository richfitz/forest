#ifndef _FOREST_MODELS_GAUSSIAN_HPP_
#define _FOREST_MODELS_GAUSSIAN_HPP_

#include <R.h>      // M_PI
#include <vector>   // std::vector
#include "../util.hpp" // stop

namespace forest {
namespace models {

struct gaussian {
  gaussian(double mean_, double variance_, double log_scale_)
    : mean(mean_), variance(variance_), log_scale(log_scale_) {}
  // Useful from R:
  gaussian(const std::vector<double>& pars)
    : mean(pars.at(0)), variance(pars.at(1)), log_scale(pars.at(2)) {
    if (pars.size() != 3) // NOTE: Only > 3 will throw here
      stop("Expected exactly three parameters");
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
    return variance > 0;
  }
  double mean;
  double variance;
  double log_scale;
};

}
}

#endif
