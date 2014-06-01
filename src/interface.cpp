#include <forest.h>

// Things additional to the API:
#include <forest/tree/ape.hpp>
#include <forest/tree/newick.hpp>

using forest::rnode;
using forest::rtree;
using forest::rsubtree;

// This is identical to the xnode description.
#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're Rcpp issues.
#pragma clang diagnostic ignored "-Wglobal-constructors"
#pragma clang diagnostic ignored "-Wexit-time-destructors"
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#endif
RCPP_MODULE(forest) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<rtree>("rtree")
    .constructor()
    .constructor<rtree::node_type>()

    .method("copy",              &rtree::copy)
    .method("clear",             &rtree::clear)

    // 1. Basic interrogation
    .property("empty",           &rtree::empty)
    .property("size",            &rtree::size)
    .property("arity",           &rtree::arity)
    .property("childless",       &rtree::childless)
    .property("representation",  &rtree::representation)

    // 2. Accessors: Provide access to root node.  I wonder if we
    // should let '[[' access children?  And should allow root
    // assignment.  But with [[, how to distinguish between subtrees
    // and nodes of subtrees?  Probably return subtrees by default.
    //
    // TODO: Refuse to return root node from empty tree.
    .property("root_node",       &rtree::root,
              &rtree::set_root_node)
    .method("[[",                &rtree::r_at)
    .method("[[<-",              &rtree::r_insert_at)

    .method("equals",            &rtree::operator==)

    .property("tips",            &rtree::tips)
    .property("nodes",           &rtree::nodes)
    .property("tip_labels",      &rtree::tip_labels)
    .property("node_labels",     &rtree::node_labels)
    .property("heights",         &rtree::heights)
    .property("depths",          &rtree::depths)
    .property("is_binary",       &rtree::is_binary)
    .property("has_branch_lengths",
              &rtree::has_branch_lengths)
    .method("is_ultrametric",    &rtree::is_ultrametric)
    .method("update_heights",    &rtree::update_heights)

    .method("collapse_singles",  &rtree::collapse_singles)
    .method("drop_tips",         &rtree::drop_tips)
    .method("rotate",            &rtree::rotate)
    .method("ladderise",         &rtree::ladderise)
    .method("get_subtree",       &rtree::get_subtree)

    .method("check_names",       &rtree::check_names)
    .method("associate_data",    &rtree::associate_data)
    .method("copy_structure",    &rtree::copy_structure)
    ;

  Rcpp::class_<rsubtree>("rsubtree")
    // NOTE: no constructor, copy, clear
    // 1. Basic interrogation
    .property("empty",           &rsubtree::empty)
    .property("size",            &rsubtree::size)
    .property("arity",           &rsubtree::arity)
    .property("childless",       &rsubtree::childless)
    .property("representation",  &rsubtree::representation)

    // 2. Accessors: Provide access to root node.  I wonder if we
    // should let '[[' access children?  And should allow root
    // assignment.  But with [[, how to distinguish between subtrees
    // and nodes of subtrees?  Probably return subtrees by default.
    .property("root_node",       &rsubtree::root,
              &rsubtree::set_root_node)
    .method("[[",                &rsubtree::r_at)
    .method("[[<-",              &rsubtree::r_insert_at)

    .method("equals",            &rsubtree::operator==)

    // Extra
    .property("tips",            &rsubtree::tips)
    .property("nodes",           &rsubtree::nodes)
    .property("tip_labels",      &rsubtree::tip_labels)
    .property("node_labels",     &rsubtree::node_labels)
    .property("heights",         &rsubtree::heights)
    .property("depths",          &rsubtree::depths)

    .method("rotate",            &rsubtree::rotate)
    .method("get_subtree",       &rsubtree::get_subtree)
    .method("to_tree",           &rsubtree::to_tree)
    ;
}

// TODO: Can set number of digits here as a default.
// TODO: Check int signedness
// [[Rcpp::export]]
std::string
to_newick_string(treetree::tree<forest::rtree::node_type> tr,
                 int digits) {
  return forest::to_newick_string(tr, digits);
}

// [[Rcpp::export]]
forest::rtree::node_type
from_newick_node(std::string x) {
  return forest::from_newick_node<forest::rtree::node_type>(x);
}

// TODO: Do the splitting here and save a bunch of time.
// [[Rcpp::export]]
treetree::tree<forest::rtree::node_type>
from_newick_string(const std::vector<std::string>& tokens_str) {
  return forest::from_newick_string<forest::rtree::node_type>(tokens_str);
}

// TODO: I don't see why the ape stuff is templated.  Could just do
// this for the one type and shift it into C++ code here.

// TODO: be careful with size_t here!  Negative inputs will cause
// crashes.
// [[Rcpp::export]]
treetree::tree<forest::rtree::node_type>
from_ape_internal(const std::vector<size_t>& order,
                  const std::vector<size_t>& from,
                  const std::vector<size_t>& to,
                  const std::vector<std::string>& label,
                  const std::vector<double>& length) {
  typedef forest::rtree::node_type node_type;
  using forest::from_ape_internal;
  return from_ape_internal<node_type>(order, from, to, label, length);
}

// TODO: Can probably sort out things like class information here,
// too.
// [[Rcpp::export]]
Rcpp::List
to_ape_internal(const treetree::tree<forest::rtree::node_type>& tr) {
  return forest::to_ape_internal(tr);
}

// TODO: This can be templated against node type and moved into a
// general utilities section.  Needs to be added after Rcpp is loaded
// though.  More usefully, treeapply (on the R side, which this uses)
// can be pushed through here.
// [[Rcpp::export]]
Rcpp::List
drain_tree(const treetree::tree<forest::rtree::node_type>& tr) {
  typedef forest::rtree::node_type node_type;
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
classify(const treetree::tree<forest::rtree::node_type>& tr,
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
