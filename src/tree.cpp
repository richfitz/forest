#include <forest.h>

// TODO: [[ should return a node and [ should return a subtree?
// TODO: get_node to match get_subtree, or wrap both together as
// something nice.  Need corresponding set functions too.

// [[Rcpp::export]]
forest::forest_tree forest_tree__ctor_empty() {
  return forest::forest_tree();
}
// [[Rcpp::export]]
forest::forest_tree forest_tree__ctor_node(forest::forest_node nd) {
  return forest::forest_tree(nd);
}

// Next big issue: How we deal with subtrees.  Punting on this for
// now, but so many functions cope with both that we should probably
// export just one type and flag if it's a subtree or not, maybe?

// The other issue is dealing with tree types that are different.
// Passing in a plotting tree for this treatment will induce a
// copy/convert I think.

// For these methods we can do this easily with cog as
//   expose_const('forest::forest_tree', 'empty')
//   expose_const('forest::forest_tree', 'size')
// etc.  That's not that much worse than what was in the modules code.

/*[[[cog
from generation import *
header()
# Basic information:
export_cr_method('forest_tree', 'empty', 'bool')
export_cr_method('forest_tree', 'size', 'size_t')
export_cr_method('forest_tree', 'arity', 'size_t')
export_cr_method('forest_tree', 'childless', 'bool')
export_cr('forest_tree', 'representation', 'std::string')

export_ptr_method('forest_tree', 'clear', 'void')

export_cr('forest_tree', 'count_tips', 'size_t')
export_cr('forest_tree', 'count_nodes', 'size_t')
export_cr('forest_tree', 'tip_labels', 'std::vector<std::string>')
export_cr('forest_tree', 'node_labels', 'std::vector<std::string>')
export_cr('forest_tree', 'heights', 'std::vector<double>')
export_cr('forest_tree', 'depths', 'std::vector<double>')
export_cr('forest_tree', 'is_binary',          'bool')
export_cr('forest_tree', 'has_branch_lengths', 'bool')
export_cr('forest_tree', 'is_ultrametric',     'bool', [('double', 'eps')])

export_ptr('forest_tree', 'update_heights')
export_ptr('forest_tree', 'collapse_singles')
export_ptr('forest_tree', 'ladderise',
           args=[('bool', 'right')])
export_ptr('forest_tree', 'drop_tips_by_label',
           args=[('const std::vector<std::string>&', 'labels')])
export_ptr('forest_tree', 'rotate', args=[('std::string', 'label')])

export_cr('forest_tree', 'check_names', 'bool',
          [('const std::vector<std::string>&', 'labels'),
           ('bool', 'tip'), ('bool', 'node')])

export_ptr('forest_tree', 'associate_data',
           args=[('SEXP', 'data'), ('bool', 'tip'), ('bool', 'node')])

# TODO: equals(?)

]]]*/
// *** Generated section: do not edit until the end marker
// [[Rcpp::export]]
bool forest_tree__empty(const forest::forest_tree& tr) {
  return tr.empty();
}
// [[Rcpp::export]]
size_t forest_tree__size(const forest::forest_tree& tr) {
  return tr.size();
}
// [[Rcpp::export]]
size_t forest_tree__arity(const forest::forest_tree& tr) {
  return tr.arity();
}
// [[Rcpp::export]]
bool forest_tree__childless(const forest::forest_tree& tr) {
  return tr.childless();
}
// [[Rcpp::export]]
std::string forest_tree__representation(const forest::forest_tree& tr) {
  return forest::representation(tr);
}
// [[Rcpp::export]]
void forest_tree__clear(Rcpp::RObject x) {
  Rcpp::XPtr<forest::forest_tree> ptr =
    forest::exporters::ptr_tree_from_R<forest::forest_node>(x);
  ptr->clear();
}
// [[Rcpp::export]]
size_t forest_tree__count_tips(const forest::forest_tree& tr) {
  return forest::count_tips(tr);
}
// [[Rcpp::export]]
size_t forest_tree__count_nodes(const forest::forest_tree& tr) {
  return forest::count_nodes(tr);
}
// [[Rcpp::export]]
std::vector<std::string> forest_tree__tip_labels(const forest::forest_tree& tr) {
  return forest::tip_labels(tr);
}
// [[Rcpp::export]]
std::vector<std::string> forest_tree__node_labels(const forest::forest_tree& tr) {
  return forest::node_labels(tr);
}
// [[Rcpp::export]]
std::vector<double> forest_tree__heights(const forest::forest_tree& tr) {
  return forest::heights(tr);
}
// [[Rcpp::export]]
std::vector<double> forest_tree__depths(const forest::forest_tree& tr) {
  return forest::depths(tr);
}
// [[Rcpp::export]]
bool forest_tree__is_binary(const forest::forest_tree& tr) {
  return forest::is_binary(tr);
}
// [[Rcpp::export]]
bool forest_tree__has_branch_lengths(const forest::forest_tree& tr) {
  return forest::has_branch_lengths(tr);
}
// [[Rcpp::export]]
bool forest_tree__is_ultrametric(const forest::forest_tree& tr, double eps) {
  return forest::is_ultrametric(tr, eps);
}
// [[Rcpp::export]]
void forest_tree__update_heights(Rcpp::RObject x) {
  Rcpp::XPtr<forest::forest_tree> ptr =
    forest::exporters::ptr_tree_from_R<forest::forest_node>(x);
  forest::update_heights(*ptr);
}
// [[Rcpp::export]]
void forest_tree__collapse_singles(Rcpp::RObject x) {
  Rcpp::XPtr<forest::forest_tree> ptr =
    forest::exporters::ptr_tree_from_R<forest::forest_node>(x);
  forest::collapse_singles(*ptr);
}
// [[Rcpp::export]]
void forest_tree__ladderise(Rcpp::RObject x, bool right) {
  Rcpp::XPtr<forest::forest_tree> ptr =
    forest::exporters::ptr_tree_from_R<forest::forest_node>(x);
  forest::ladderise(*ptr, right);
}
// [[Rcpp::export]]
void forest_tree__drop_tips_by_label(Rcpp::RObject x, const std::vector<std::string>& labels) {
  Rcpp::XPtr<forest::forest_tree> ptr =
    forest::exporters::ptr_tree_from_R<forest::forest_node>(x);
  forest::drop_tips_by_label(*ptr, labels);
}
// [[Rcpp::export]]
void forest_tree__rotate(Rcpp::RObject x, std::string label) {
  Rcpp::XPtr<forest::forest_tree> ptr =
    forest::exporters::ptr_tree_from_R<forest::forest_node>(x);
  forest::rotate(*ptr, label);
}
// [[Rcpp::export]]
bool forest_tree__check_names(const forest::forest_tree& tr, const std::vector<std::string>& labels, bool tip, bool node) {
  return forest::check_names(tr, labels, tip, node);
}
// [[Rcpp::export]]
void forest_tree__associate_data(Rcpp::RObject x, SEXP data, bool tip, bool node) {
  Rcpp::XPtr<forest::forest_tree> ptr =
    forest::exporters::ptr_tree_from_R<forest::forest_node>(x);
  forest::associate_data(*ptr, data, tip, node);
}
//[[[end]]]

// This one here is not done through code generation because it's bit
// weird.  It might be worth a function that does copy_structure for
// T->T, but not sure.  Because of the issues around overloading
// functions with different return types we'd probably need a
// different name.
// [[Rcpp::export]]
forest::forest_tree
forest_tree__copy_structure(const forest::forest_tree& tr) {
  typedef forest::forest_node T;
  return forest::copy_structure<T>(treetree::const_subtree<T>(tr));
}

// This one is odd.
// [[Rcpp::export]]
forest::forest_tree forest_tree__copy(const forest::forest_tree& tr) {
  forest::forest_tree tr2(tr); // may not actually be needed.
  return tr2;
}

// NOTE: No copy method for subtrees because they can't be copied
// (they'd be a pointer into a tree that does not exist).  It's
// possible actually that we could recover and copy the underlying
// tree *from* the subtree, copy that and return a subtree pointing at
// the same place.  That's a bit of work to set up though, and has
// potentially odd semantics.

// Subtrees

/*[[[cog
from generation import *
header()
# Basic information:
export_cr_method('forest_subtree', 'empty', 'bool')
export_cr_method('forest_subtree', 'size', 'size_t')
export_cr_method('forest_subtree', 'arity', 'size_t')
export_cr_method('forest_subtree', 'childless', 'bool')
export_cr('forest_subtree', 'representation', 'std::string')

export_cr('forest_subtree', 'count_tips', 'size_t')
export_cr('forest_subtree', 'count_nodes', 'size_t')
export_cr('forest_subtree', 'tip_labels', 'std::vector<std::string>')
export_cr('forest_subtree', 'node_labels', 'std::vector<std::string>')
export_cr('forest_subtree', 'subtree_to_tree', 'forest::forest_tree')
]]]*/
// *** Generated section: do not edit until the end marker
// [[Rcpp::export]]
bool forest_subtree__empty(const forest::forest_subtree& tr) {
  return tr.empty();
}
// [[Rcpp::export]]
size_t forest_subtree__size(const forest::forest_subtree& tr) {
  return tr.size();
}
// [[Rcpp::export]]
size_t forest_subtree__arity(const forest::forest_subtree& tr) {
  return tr.arity();
}
// [[Rcpp::export]]
bool forest_subtree__childless(const forest::forest_subtree& tr) {
  return tr.childless();
}
// [[Rcpp::export]]
std::string forest_subtree__representation(const forest::forest_subtree& tr) {
  return forest::representation(tr);
}
// [[Rcpp::export]]
size_t forest_subtree__count_tips(const forest::forest_subtree& tr) {
  return forest::count_tips(tr);
}
// [[Rcpp::export]]
size_t forest_subtree__count_nodes(const forest::forest_subtree& tr) {
  return forest::count_nodes(tr);
}
// [[Rcpp::export]]
std::vector<std::string> forest_subtree__tip_labels(const forest::forest_subtree& tr) {
  return forest::tip_labels(tr);
}
// [[Rcpp::export]]
std::vector<std::string> forest_subtree__node_labels(const forest::forest_subtree& tr) {
  return forest::node_labels(tr);
}
// [[Rcpp::export]]
forest::forest_tree forest_subtree__subtree_to_tree(const forest::forest_subtree& tr) {
  return forest::subtree_to_tree(tr);
}
//[[[end]]]
