#ifndef _FOREST_MODELS_HPP_
#define _FOREST_MODELS_HPP_

#include <cstdlib>
#include <vector>

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
struct gaussian {
  gaussian(double mean_, double variance_, double scale_)
    : mean(mean_), variance(variance_), scale(scale_) {}
  // could have a validate method.
  static size_t size() {return 3;}
  double mean;
  double variance;
  double scale;
};

struct brownian_motion { // no drift
  brownian_motion() : s2(0.0) {} // NA_REAL better?
  // TODO: in-place or reference version to avoid copy?
  gaussian forward(gaussian y, double t) const {
    y.variance += t * s2;
    return y;
  }
  gaussian backward(gaussian y, double t) const {
    return forward(y, t);
  }

  std::vector<double> parameters() const {
    return std::vector<double>(1, s2);
  }
  void set_parameters(std::vector<double> parameters) {
    // TODO: check length 1
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
