#include "tree/plotting.hpp"
#include <boost/utility.hpp> // boost::prior

namespace forest {
namespace plotting {

// Support for moving the plot_info class in and out of R.  This is
// just done as a list, so potentially not super efficient.
//
// However, this might change so that we return a matrix instead as
// we'll generally want this for all of the species in the tree at
// once.  That might need some modification for subtrees?
plot_info::plot_info(SEXP obj) {
  Rcpp::List tmp = Rcpp::as<Rcpp::List>(obj);
  time_tipward  = Rcpp::as<double>(tmp["time_tipward"]);
  time_rootward = Rcpp::as<double>(tmp["time_rootward"]);
  spacing_min   = Rcpp::as<double>(tmp["spacing_min"]);
  spacing_max   = Rcpp::as<double>(tmp["spacing_max"]);
  spacing_mid   = Rcpp::as<double>(tmp["spacing_mid"]);
  is_tip        = Rcpp::as<bool>(tmp["is_tip"]);
}
plot_info::operator SEXP() const {
  using namespace Rcpp;
  List ret = List::create(_["time_tipward"]  = time_tipward,
                          _["time_rootward"] = time_rootward,
                          _["spacing_min"]   = spacing_min,
                          _["spacing_max"]   = spacing_max,
                          _["spacing_mid"]   = spacing_mid,
                          _["is_tip"]        = is_tip);
  return Rcpp::wrap(ret);
}

// Useful typedefs to avoid typing (not exported yet).
typedef node<Rcpp::RObject>          node_robject;
typedef node<plot_info>              node_plot;
typedef treetree::tree<node_robject> tree_robject;
typedef treetree::tree<node_plot>    tree_plot;

// There are several sets of coordinates that need sorting out, but
// before that: some definitions.

// Time axis -- this is ape's node_depths.  This (perhaps with
// transformation) will be the distance from the centre in a radial
// plot and the x distance in a default plotted non-radial plot.

// Spacing axis -- this is ape's node_heights.  This is a little bit
// harder as the

// I'm not sure how ape deals with lines crossing or not.  Perhaps
// that never happens?  But it seems like it could.  I suspect that
// ladderising will always fix it, but there may be other things going
// on.
//
// I'm not hugely interested in the "clado" style of plot; it looks
// poor with branch lengths.

// There are a ton of options for computing some of the numbers here;
// we could either sort these out via
//   * a small struct of options
//   * actually passing them into the functions (less keen on that)
//   * computing all the possible options and leaving the decisions to
//     the R side.  Not coding any options for now though.

// At the moment I'm working with an R list but a small struct might
// be easier to work with actually.  We could then do the struct ->
// Rcpp::List conversion on export.  That would have the advantage of
// a little extra type safety and self-documentation about what we're
// actually building here.

tree_plot coordinates(const tree_robject& tree) {
  // This cleans out all the existing data into a copy: It might not
  // actually be a good idea to do this (what if we want to *do*
  // something with the tree's data (like trait plot).  But that's a
  // question for later on as there are too many different ways that
  // could go to get this correct now.  Another option would be to
  // copy the data into a 'data' element within this list.
  tree_plot ret(copy_structure<tree_plot::value_type>(tree));
  coordinates_time(ret);
  coordinates_spacing(ret);
  return ret;
}

// We already have the time axis done; that is 'height' and can just
// be copied over.  This will fail for trees without branch lengths,
// and we should do something else here.
//
// Time can be measured up or down the tree, but we'll just measure up
// time here (height) becuase it's unambiguous.
//
//       +------------------
//       |
// ------+             +----
//       |             |
//       +-------------+----
//       ^             ^
//       t_rootward    t_tipward
//
void coordinates_time(tree_plot& tree) {
  update_heights(tree);
  for (tree_plot::pre_iterator it = tree.begin(); it != tree.end(); ++it) {
    it->data_.time_tipward  = it->height_;
    it->data_.time_rootward = it->height_ - it->length_;
  }
}

// For 'spacing' there are a couple of different things that might be
// useful.  For an internal node there are three measures that we
// want: s_min and s_max are the spacing-axis values for the lowest
// and highest offspring nodes and s_mid is the spacing-axis value
// that the branch comes in.  I'm only going to implement s_mid =
// (s_min + s_max)/2 but other values are possible that take into
// account things like how diverse different clades are (so weight
// s_min and s_max by the relative diversities of their clades for
// example).  For a node, s_min = s_max = s_mid.
//
//        s_max  +-----
//               |
//  s_mid -------+
//               |
//        s_min  +----
//
// In the returned object note that `s_` is `spacing_`.
//
// NOTE: In contrast with ape, where tips are spaced as {1, 2, n_tip},
// I am giving tips coordinates {0.0, ..., 1.0} as that is going to be
// easier to scale.
//
// NOTE: tips and internal calculations are separated out because:
//   * tips need to be handled differently for normal and "clade"
//     trees
//   * different node positioning algorithms affect only internals and
//     affect normal and clade trees equally.
void coordinates_spacing(tree_plot& tree) {
  coordinates_spacing_tips(tree);
  coordinates_spacing_internal(tree);
}

void coordinates_spacing_tips(tree_plot& tree) {
  size_t tip = 0, n_tip = count_tips(tree);
  for (tree_plot::sub_post_iterator it = tree.begin_sub_post();
       it != tree.end_sub_post(); ++it) {
    tree_plot::post_iterator nd = it;
    const double ds = 1 / static_cast<double>(n_tip - 1);
    nd->data_.is_tip = it->childless();
    if (nd->data_.is_tip) {
      const double s = tip * ds;
      nd->data_.spacing_min = s;
      nd->data_.spacing_max = s;
      nd->data_.spacing_mid = s;
      ++tip;
    }
  }
}
void coordinates_spacing_internal(tree_plot& tree) {
  for (tree_plot::sub_post_iterator it = tree.begin_sub_post();
       it != tree.end_sub_post(); ++it) {
    tree_plot::post_iterator nd = it;
    if (!it->childless()) {
      nd->data_.spacing_min = it->begin_child()->data_.spacing_mid;
      nd->data_.spacing_max = boost::prior(it->end_child())->data_.spacing_mid;
      nd->data_.spacing_mid = (nd->data_.spacing_min +
                               nd->data_.spacing_max)/2.0;
    }
  }
}

}
}
