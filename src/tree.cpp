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
export_const_method('forest_tree', 'empty', 'bool')
export_const_method('forest_tree', 'size', 'size_t')
export_const_method('forest_tree', 'arity', 'size_t')
export_const_method('forest_tree', 'childless', 'bool')
export_const_function('forest_tree', 'representation', 'std::string')

export_const_function('forest_tree', 'count_tips', 'size_t')
export_const_function('forest_tree', 'count_nodes', 'size_t')
export_const_function('forest_tree', 'tip_labels', 'std::vector<std::string>')
export_const_function('forest_tree', 'node_labels', 'std::vector<std::string>')
export_const_function('forest_tree', 'heights', 'std::vector<double>')
export_const_function('forest_tree', 'depths', 'std::vector<double>')
export_const_function('forest_tree', 'is_binary',          'bool')
export_const_function('forest_tree', 'has_branch_lengths', 'bool')
export_const_function('forest_tree', 'is_ultrametric',     'bool',
                      [('double', 'eps')])

export_nonconst('forest_tree', 'update_heights')
export_nonconst('forest_tree', 'collapse_singles')
export_nonconst('forest_tree', 'ladderise', [('bool', 'right')])
export_nonconst('forest_tree', 'drop_tips_by_label',
                [('const std::vector<std::string>&', 'labels')])
export_nonconst('forest_tree', 'rotate', [('std::string', 'label')])

export_const_function('forest_tree', 'check_names', 'bool',
                      [('const std::vector<std::string>&', 'labels'),
                       ('bool', 'tip'), ('bool', 'node')])

export_nonconst('forest_tree', 'associate_data',
                [('SEXP', 'data'), ('bool', 'tip'), ('bool', 'node')])

# TODO: copy
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
void forest_tree__update_heights(Rcpp::XPtr<forest::forest_tree> ptr) {
  forest::util::check_ptr_valid(ptr);
  forest::update_heights(*ptr);
}
// [[Rcpp::export]]
void forest_tree__collapse_singles(Rcpp::XPtr<forest::forest_tree> ptr) {
  forest::util::check_ptr_valid(ptr);
  forest::collapse_singles(*ptr);
}
// [[Rcpp::export]]
void forest_tree__ladderise(Rcpp::XPtr<forest::forest_tree> ptr, bool right) {
  forest::util::check_ptr_valid(ptr);
  forest::ladderise(*ptr, right);
}
// [[Rcpp::export]]
void forest_tree__drop_tips_by_label(Rcpp::XPtr<forest::forest_tree> ptr, const std::vector<std::string>& labels) {
  forest::util::check_ptr_valid(ptr);
  forest::drop_tips_by_label(*ptr, labels);
}
// [[Rcpp::export]]
void forest_tree__rotate(Rcpp::XPtr<forest::forest_tree> ptr, std::string label) {
  forest::util::check_ptr_valid(ptr);
  forest::rotate(*ptr, label);
}
// [[Rcpp::export]]
bool forest_tree__check_names(const forest::forest_tree& tr, const std::vector<std::string>& labels, bool tip, bool node) {
  return forest::check_names(tr, labels, tip, node);
}
// [[Rcpp::export]]
void forest_tree__associate_data(Rcpp::XPtr<forest::forest_tree> ptr, SEXP data, bool tip, bool node) {
  forest::util::check_ptr_valid(ptr);
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

// This one also takes a pointer and returns non-void.  It's important
// to take the pointer because we need to keep the link to the tree,
// *I think* (TODO: check this)
// //
// // [[Rcpp::export]]
// forest::forest_subtree
// forest_tree__child_subtree(Rcpp::XPtr<forest::forest_tree> ptr, int idx) {
//   forest::util::check_ptr_valid(ptr);
//   size_t i = forest::util::safe_index_from_r(idx, ptr->arity());
//   return forest::child_subtree(*ptr, i);
// }

// // [[Rcpp::export]]
// forest::forest_node
// forest_tree__child_node(Rcpp::XPtr<forest::forest_tree> ptr, int idx) {
//   forest::util::check_ptr_valid(ptr);
//   size_t i = forest::util::safe_index_from_r(idx, ptr->arity());
//   return forest::child_node(*ptr, i);
// }

// // These could be done with export_nonconst()

// // [[Rcpp::export]]
// void
// forest_tree__replace_child_subtree(Rcpp::XPtr<forest::forest_tree> ptr,
//                                    size_t idx,
//                                    const forest::forest_subtree& value) {
//   forest::util::check_ptr_valid(ptr);
//   size_t i = forest::util::safe_index_from_r(idx, ptr->arity());
//   forest::replace_child_subtree(*ptr, i, value);
// }

// // [[Rcpp::export]]
// void
// forest_tree__replace_child_node(Rcpp::XPtr<forest::forest_tree> ptr,
//                                 size_t idx,
//                                 const forest::forest_node& value) {
//   forest::util::check_ptr_valid(ptr);
//   size_t i = forest::util::safe_index_from_r(idx, ptr->arity());
//   forest::replace_child_node(*ptr, i, value);
// }

// // [[Rcpp::export]]
// void
// forest_subtree__replace_child_subtree(Rcpp::XPtr<forest::forest_subtree> ptr,
//                                       size_t idx,
//                                       const forest::forest_subtree& value) {
//   forest::util::check_ptr_valid(ptr);
//   size_t i = forest::util::safe_index_from_r(idx, ptr->arity());
//   forest::replace_child_subtree(*ptr, i, value);
// }

// // [[Rcpp::export]]
// void
// forest_subtree__replace_child_node(Rcpp::XPtr<forest::forest_subtree> ptr,
//                                    size_t idx,
//                                    const forest::forest_node& value) {
//   forest::util::check_ptr_valid(ptr);
//   size_t i = forest::util::safe_index_from_r(idx, ptr->arity());
//   forest::replace_child_node(*ptr, i, value);
// }

// This one is odd because it takes a pointer but runs a member
// function
// [[Rcpp::export]]
void forest_tree__clear(Rcpp::XPtr<forest::forest_tree> ptr) {
  forest::util::check_ptr_valid(ptr);
  ptr->clear();
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

// TODO: implement to_tree -- see old wrapper

// [[Rcpp::export]]
forest::forest_subtree forest_subtree__copy(const forest::forest_subtree& tr) {
  forest::forest_subtree tr2(tr); // may not actually be needed.
  return tr2;
}

// Subtrees

/*[[[cog
from generation import *
header()
# Basic information:
export_const_method('forest_subtree', 'empty', 'bool')
export_const_method('forest_subtree', 'size', 'size_t')
export_const_method('forest_subtree', 'arity', 'size_t')
export_const_method('forest_subtree', 'childless', 'bool')
export_const_function('forest_subtree', 'representation', 'std::string')

export_const_function('forest_subtree', 'count_tips', 'size_t')
export_const_function('forest_subtree', 'count_nodes', 'size_t')
export_const_function('forest_subtree', 'tip_labels', 'std::vector<std::string>')
export_const_function('forest_subtree', 'node_labels', 'std::vector<std::string>')
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
//[[[end]]]

// Should be fine with code generation:

// [[Rcpp::export]]
forest::forest_tree
forest_subtree__to_tree(const forest::forest_subtree& tr) {
  return subtree_to_tree(tr);
}
