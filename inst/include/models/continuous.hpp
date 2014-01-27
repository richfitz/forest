#ifndef _FOREST_MODELS_CONTINUOUS_HPP_
#define _FOREST_MODELS_CONTINUOUS_HPP_

// Support for models of continuous trait evolution that involve
// Gaussian distributions at nodes.

// NOTE: Because this requires some use of Rcpp types, this is
// something that will need to be included in models.hpp, not in
// common.hpp.
#include "tree/common.hpp"
#include "models/common.hpp"
#include <Rcpp.h>

namespace forest {
namespace models {

// These will be useful, but for now restrict them to this file.
// Because the types are naturally nested, these would be a pain to
// write:
//   treetree::tree<node<branch_pair<gaussian> > >
namespace {
typedef branch_pair<gaussian> gpair;
typedef forest::node<gpair>   gnode;
typedef treetree::tree<gnode> gtree;
}

// Go through and check that all data are reasonable.  This is
// actually something that could be easier to do on the R side, but
// doing it here means we can use it within the main code.  Probably
// another function that goes through and does some handholding as to
// where the missing data belong is important medium-term.
//
// The other option is do do all this checking and build a tree at the
// same time?
//
// NOTE: This is written as a template, but the only type that we'll
// actually run this for is treetree::tree< node<Rcpp::RObject> >
//
// NOTE: There is some duplication here with to_rtree(); could
// probably share code if I can work out how to template the
// translation bit.

// At the risk of being implicit over explicit. Also, this behaviour
// would be better in the gaussian class I think.  But this function
// can just move there, perhaps as a static function, used by a
// constructor?
inline gpair gaussian_pair_from_R(SEXP obj);

template <typename T>
gtree build_gaussian_tree(const treetree::tree<T>& tree) {
  gtree ret = forest::duplicate_topology<gnode,T>(tree);
  typename treetree::tree<T>::const_pre_iterator it = tree.begin();
  typename treetree::tree<gnode>::pre_iterator it_to = ret.begin();
  while (it != tree.end()) {
    if (it->data_ != R_NilValue)
      it_to->data_ = gaussian_pair_from_R(it->data_);
    ++it_to;
    ++it;
  }
  return ret;
}

inline gpair gaussian_pair_from_R(SEXP obj) {
  std::vector<double> p = Rcpp::as<std::vector<double> >(obj);
  if (p.size() == 0)
    stop("Missing a mean");
  else if (p.size() < 3)
    p.resize(3, 0.0); // variance, log_scale
  // NOTE: pairs are (currently) initialised as tipward/rootward.  But
  // this is counter intuitive and I might change it.
  return branch_pair<gaussian>(gaussian(p), gaussian());
}

}
}

#endif
