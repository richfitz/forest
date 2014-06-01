#include <forest.h>

// Things additional to the API:
#include <forest/tree/ape.hpp>
#include <forest/tree/newick.hpp>

// TODO: Can set number of digits here as a default.
// TODO: Check int signedness
// [[Rcpp::export]]
std::string
to_newick_string(treetree::tree<forest::forest_node> tr,
                 int digits) {
  return forest::to_newick_string(tr, digits);
}

// [[Rcpp::export]]
forest::forest_node
from_newick_node(std::string x) {
  return forest::from_newick_node<forest::forest_node>(x);
}

// TODO: Do the splitting here and save a bunch of time.
// [[Rcpp::export]]
treetree::tree<forest::forest_node>
from_newick_string(const std::vector<std::string>& tokens_str) {
  return forest::from_newick_string<forest::forest_node>(tokens_str);
}

// TODO: I don't see why the ape stuff is templated.  Could just do
// this for the one type and shift it into C++ code here.

// TODO: be careful with size_t here!  Negative inputs will cause
// crashes.
// [[Rcpp::export]]
treetree::tree<forest::forest_node>
from_ape_internal(const std::vector<size_t>& order,
                  const std::vector<size_t>& from,
                  const std::vector<size_t>& to,
                  const std::vector<std::string>& label,
                  const std::vector<double>& length) {
  typedef forest::forest_node node_type;
  using forest::from_ape_internal;
  return from_ape_internal<node_type>(order, from, to, label, length);
}

// TODO: Can probably sort out things like class information here,
// too.
// [[Rcpp::export]]
Rcpp::List
to_ape_internal(const treetree::tree<forest::forest_node>& tr) {
  return forest::to_ape_internal(tr);
}

// TODO: This can be templated against node type and moved into a
// general utilities section.  Needs to be added after Rcpp is loaded
// though.  More usefully, treeapply (on the R side, which this uses)
// can be pushed through here.
// [[Rcpp::export]]
Rcpp::List
drain_tree(const treetree::tree<forest::forest_node>& tr) {
  typedef forest::forest_node node_type;
  Rcpp::List ret;
  for (treetree::tree<node_type>::const_pre_iterator
         it = tr.begin(); it != tr.end(); ++it)
    ret.push_back(Rcpp::wrap(*it));
  return ret;
}

// TODO: Might be worth losing the r_classify function and pushing the
// meat into here?  If it's not used elsewhere that might better.
// [[Rcpp::export]]
Rcpp::IntegerVector
classify(const treetree::tree<forest::forest_node>& tr,
         const std::vector<std::string>& labels) {
  return forest::r_classify(tr, labels);
}

// [[Rcpp::export]]
treetree::tree<forest::node<forest::plotting::plot_info> >
plotting_coordinates(const treetree::tree<forest::node<Rcpp::RObject> >& tree) {
  return forest::plotting::coordinates(tree);
}

// [[Rcpp::export]]
treetree::tree<forest::node<forest::plotting::plot_info> >
plotting_coordinates_clade(const treetree::tree<forest::node<Rcpp::RObject> >& tree, const std::vector<double>& n_taxa, double p) {
  return forest::plotting::coordinates_clade(tree, n_taxa, p);
}
