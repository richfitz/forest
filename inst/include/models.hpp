#ifndef _FOREST_MODELS_HPP_
#define _FOREST_MODELS_HPP_

#include <cstdlib>
#include <vector>
#include <R.h> // pi
#include "models/gaussian.hpp"

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

// In terms of thinking about future time-dependent models, some of
// this might need to change.  But this is possibly the lowest level
// bits.  Trying hard not to generalise too early.
//
// NOTE: that for backward/forward we pass by value because we need to
// copy things anyway.  Pass by reference might be better?

// The other way of viewing these is as convolutions: Forward in time
// convolves a BM distribution with a kernel of mean 0, scale 1 and
// variance t * s2.  With drift we convolve it with a kernel that has
// nonzero mean.  Doing it this way will further simplify the code
// below.
struct brownian_motion { // no drift
  brownian_motion() : s2(NA_REAL) {}
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

  // Probably significant repetition here amongst models; factor that
  // out as we find it.
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
