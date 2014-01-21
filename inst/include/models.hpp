#ifndef _FOREST_MODELS_HPP_
#define _FOREST_MODELS_HPP_

#include <cstdlib>

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

}
}

#endif
