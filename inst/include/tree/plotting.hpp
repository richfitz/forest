#ifndef _FOREST_TREE_PLOTTING_HPP_
#define _FOREST_TREE_PLOTTING_HPP_

#include "tree.hpp"
#include <Rcpp.h>

// NOTE: In contrast with the suggestions in Rcpp-extending, I've
// included Rcpp.h here, and not after the class definition.  This
// does not seem to matter for use though (if that was hard and fast,
// then I believe the usual EXPOSED_CLASS trick would not work?)

namespace forest {
namespace plotting {

class plot_info {
public:
  plot_info()
    : time_tipward(NA_REAL), time_rootward(NA_REAL),
      spacing_min(NA_REAL), spacing_max(NA_REAL), spacing_mid(NA_REAL),
      is_tip(false) {} // No missing value for C++ bool.
  plot_info(SEXP obj);
  operator SEXP() const;

  double time_tipward;
  double time_rootward;
  double spacing_min;
  double spacing_max;
  double spacing_mid;
  // It might be that this one disappears as it duplicates something
  // available in the containing subtree.
  bool   is_tip;
};

// Update the tree with plotting coordinates.  This does not need to
// be done via templates, so I'm doing it with plain C++ here.
treetree::tree<node<plot_info> >
coordinates(const treetree::tree<node<Rcpp::RObject> >& tree);

void coordinates_time(treetree::tree<node<plot_info> >& tree);
void coordinates_spacing_tips(treetree::tree<node<plot_info> >& tree);
void coordinates_spacing_internal(treetree::tree<node<plot_info> >& tree);

// For clade trees:
treetree::tree<node<plot_info> >
coordinates_clade(const treetree::tree<node<Rcpp::RObject> >& tree,
                  const std::vector<double>& n_taxa,
                  double p);
void coordinates_spacing_tips_clade(treetree::tree<node<plot_info> >& tree,
                                    const std::vector<double>& n_taxa,
                                    double p);
}
}

namespace Rcpp {
template<>
inline SEXP wrap(const treetree::tree<forest::node<forest::plotting::plot_info> >& obj) {
  typedef forest::node<forest::plotting::plot_info>    T_in;
  typedef forest::node<Rcpp::RObject> T_out;
  return Rcpp::wrap(forest::copy_convert<T_out,T_in>(obj));
}
}

#endif
