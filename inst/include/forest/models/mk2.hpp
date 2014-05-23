#ifndef _FOREST_MODELS_MK2_HPP_
#define _FOREST_MODELS_MK2_HPP_

#include <R.h> // NA_REAL
#include <vector>
#include <forest/models/discrete.hpp>

// Special class for the two-state Markov model because we can get to
// the solution analytically.  This could also be called the M2 model
// I think (and Lewis would probably do that).  But it is Mk2 in
// diversitree, in Goldberg and Igic 2008 and I think in Mesquite.

// NOTE: It would be nice to require a specialisation of the discrete
// data type to two states, but that might be hard to enforce at
// compile time while allowing arbitrary sized matrices to be
// generated.

// NOTE: I'm not sure if we should prefer q01/q10 or q12/q21.
// Sticking with diversitree's names for now, but may change.

// NOTE: There are several ways of computing Mk probabilities:
//
//   * For the two state case we can do the tip-root moves
//     analytically.
//   * For any number of states we can take the matrix exponential of
//     Qt and matrix multiply the probability vector by this value.
//     This will work well for small to medium Q, and there is
//     possibly code in RcppArmadillo or RcppEigen.
//   * For any number of states, but especially large numbers, we can
//     use an ODE solver to propagate the probabilities forward or
//     backward in time.  That does require a set of matrix
//     multiplications.
//
// For now, I'm just implementing the first case.  The second two will
// come under something like mk2_ode and mk2_expm and will require
// some extra code and infrastructure.

namespace forest {
namespace models {

struct mk2 {
  typedef discrete data;
  mk2() : q01(NA_REAL), q10(NA_REAL) {}

  discrete forward(discrete y, double t) const {
    const double qq = q01 + q10;
    const double eqqt = exp(qq * t);
    discrete ret(y);
    ret[0] = (q01 * y[0] + q10 * (eqqt * (y[0] + y[1]) - y[1])) / (eqqt * qq);
    ret[1] = (q10 * y[1] + q01 * (eqqt * (y[0] + y[1]) - y[0])) / (eqqt * qq);
    return ret;
  }

  discrete backward(discrete y, double t) const {
    const double qq = q01 + q10; // Constant for any y
    // Component parts of the probability.  First is related to the
    // equilibrium distribution (it is the long run probability of the
    // data as time goes to infinity), second related to probability
    // of movement.
    const double tmp1 = q10 * y[0] + q01 * y[1];
    const double eqqt = exp(-qq * t);
    discrete ret(y);
    ret[0] = (tmp1 + q01*(y[0] - y[1]) * eqqt)/qq;
    ret[1] = (tmp1 + q10*(y[1] - y[0]) * eqqt)/qq;
    return ret;
  }
  discrete combine(const discrete& x, const discrete &y) const {
    return x * y;
  }

  // Again, holding off on doing this nicely until I think more
  // carefully about this.
  std::vector<double> parameters() const {
    std::vector<double> ret;
    ret.push_back(q01);
    ret.push_back(q10);
    return ret;
  }
  void set_parameters(std::vector<double> parameters) {
    // TODO: validate parameters
    util::check_length(parameters.size(), 2);
    q01 = parameters[0];
    q10 = parameters[1];
  }

private:
  double q01;
  double q10;
};

}
}

#endif
