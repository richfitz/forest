#ifndef _FOREST_MODELS_HPP_
#define _FOREST_MODELS_HPP_

#include <cstdlib>
#include <vector>
#include <R.h> // pi

namespace forest {
namespace models {

// Possibly best to think in terms of different sorts of underlying
// data; there are only a few types:
//
//   Gaussians (BM, OU, EB, etc).
//   Multistate (Mk - bunch of D values)
//   Speciation/Extinction (BiSSE, MuSSE, possibly QuaSSE)
//
// Then there is a little less poking about with indices.  Will be
// interesting to compare versions with this idea and without for
// speed though.
//
// NOTE: I think that 2pi is a constant already.
struct gaussian {
  gaussian(double mean_, double variance_, double log_scale_)
    : mean(mean_), variance(variance_), log_scale(log_scale_) {}
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
  // could have a validate method.
  static size_t size() {return 3;}
  double mean;
  double variance;
  double log_scale;
};

// In terms of thinking about future time-dependent models, some of
// this might need to change.  But this is possibly the lowest level
// bits.  Trying hard not to generalise too early.
//
// NOTE: that for backward/forward we pass by value because we need to
// copy things anyway.  Pass by reference might be better?
struct brownian_motion { // no drift
  brownian_motion() : s2(0.0) {} // NA_REAL better?
  gaussian forward(gaussian y, double t) const {
    y.variance += t * s2;
    return y;
  }
  gaussian backward(gaussian y, double t) const {
    return forward(y, t); // BM is reversible.
  }
  gaussian combine(const gaussian& x, const gaussian& y) const {
    return x * y;
  }

  std::vector<double> parameters() const {
    return std::vector<double>(1, s2);
  }
  void set_parameters(std::vector<double> parameters) {
    // TODO: check length 1
    // TODO: validate parameters
    s2 = parameters.front();
  }
  // TODO: r_parameters() that returns named parameter vector?
  // TODO: length of parameter vector

private:
  double s2;
};

}
}

#endif
