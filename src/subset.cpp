// Subsetting functions.  None of this file set up to work with code
// generation yet.
#include <forest.h>

/*[[[cog
from generation import *
header()

# TODO: Most of the value types become 'const T&'

# 0. root_subtree: get / set root subtree of a tree / subtree
export_ptr('forest_tree',    'root_subtree', 'forest::forest_subtree')
export_ptr('forest_subtree', 'root_subtree', 'forest::forest_subtree')
export_ptr('forest_tree',    'set_root_subtree',
           args=[('forest::forest_tree', 'value')])
export_ptr('forest_subtree', 'set_root_subtree',
           args=[('forest::forest_tree', 'value')])

# 1. root_node: get / set root node of a tree / subtree
export_ptr('forest_tree',    'root_node', 'forest::forest_node')
export_ptr('forest_subtree', 'root_node', 'forest::forest_node')
export_ptr('forest_tree',    'set_root_node',
           args=[('forest::forest_node', 'value')])
export_ptr('forest_subtree', 'set_root_node',
           args=[('forest::forest_node', 'value')])

# 2. child_subtree: get / set one of the child subtrees.
export_ptr('forest_tree',    'child_subtree', 'forest::forest_subtree',
           args=[('forest::util::index', 'idx')])
export_ptr('forest_subtree', 'child_subtree', 'forest::forest_subtree',
           args=[('forest::util::index', 'idx')])
export_ptr('forest_tree',    'set_child_subtree',
           args=[('forest::util::index', 'idx'),
                 ('forest::forest_tree', 'value')])
export_ptr('forest_subtree', 'set_child_subtree',
           args=[('forest::util::index', 'idx'),
                 ('forest::forest_tree', 'value')])

# 3. child_node: get / set one of the child nodes.
export_ptr('forest_tree',    'child_node', 'forest::forest_node',
           args=[('forest::util::index', 'idx')])
export_ptr('forest_subtree', 'child_node', 'forest::forest_node',
           args=[('forest::util::index', 'idx')])
export_ptr('forest_tree',    'set_child_node',
           args=[('forest::util::index', 'idx'),
                 ('forest::forest_node', 'value')])
export_ptr('forest_subtree', 'set_child_node',
           args=[('forest::util::index', 'idx'),
                 ('forest::forest_node', 'value')])

# 4. subtree: get / set a subtree by label
# set functions are not yet done because we need to accept argument of
# tree or subtree.  I'm also not totally sure what will happen to the
# previous subtree!  But it could be a nice thing to implement.
export_ptr('forest_tree',    'subtree', 'forest::forest_subtree',
           [('std::string', 'label')])
export_ptr('forest_subtree', 'subtree', 'forest::forest_subtree',
           [('std::string', 'label')])

]]]*/
// *** Generated section: do not edit until the end marker
// [[Rcpp::export]]
forest::forest_subtree forest_tree__root_subtree(Rcpp::XPtr<forest::forest_tree> ptr) {
  forest::util::check_ptr_valid(ptr);
  return forest::root_subtree(*ptr);
}
// [[Rcpp::export]]
forest::forest_subtree forest_subtree__root_subtree(Rcpp::XPtr<forest::forest_subtree> ptr) {
  forest::util::check_ptr_valid(ptr);
  return forest::root_subtree(*ptr);
}
// [[Rcpp::export]]
void forest_tree__set_root_subtree(Rcpp::XPtr<forest::forest_tree> ptr, forest::forest_tree value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_root_subtree(*ptr, value);
}
// [[Rcpp::export]]
void forest_subtree__set_root_subtree(Rcpp::XPtr<forest::forest_subtree> ptr, forest::forest_tree value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_root_subtree(*ptr, value);
}
// [[Rcpp::export]]
forest::forest_node forest_tree__root_node(Rcpp::XPtr<forest::forest_tree> ptr) {
  forest::util::check_ptr_valid(ptr);
  return forest::root_node(*ptr);
}
// [[Rcpp::export]]
forest::forest_node forest_subtree__root_node(Rcpp::XPtr<forest::forest_subtree> ptr) {
  forest::util::check_ptr_valid(ptr);
  return forest::root_node(*ptr);
}
// [[Rcpp::export]]
void forest_tree__set_root_node(Rcpp::XPtr<forest::forest_tree> ptr, forest::forest_node value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_root_node(*ptr, value);
}
// [[Rcpp::export]]
void forest_subtree__set_root_node(Rcpp::XPtr<forest::forest_subtree> ptr, forest::forest_node value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_root_node(*ptr, value);
}
// [[Rcpp::export]]
forest::forest_subtree forest_tree__child_subtree(Rcpp::XPtr<forest::forest_tree> ptr, forest::util::index idx) {
  forest::util::check_ptr_valid(ptr);
  return forest::child_subtree(*ptr, idx);
}
// [[Rcpp::export]]
forest::forest_subtree forest_subtree__child_subtree(Rcpp::XPtr<forest::forest_subtree> ptr, forest::util::index idx) {
  forest::util::check_ptr_valid(ptr);
  return forest::child_subtree(*ptr, idx);
}
// [[Rcpp::export]]
void forest_tree__set_child_subtree(Rcpp::XPtr<forest::forest_tree> ptr, forest::util::index idx, forest::forest_tree value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_child_subtree(*ptr, idx, value);
}
// [[Rcpp::export]]
void forest_subtree__set_child_subtree(Rcpp::XPtr<forest::forest_subtree> ptr, forest::util::index idx, forest::forest_tree value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_child_subtree(*ptr, idx, value);
}
// [[Rcpp::export]]
forest::forest_node forest_tree__child_node(Rcpp::XPtr<forest::forest_tree> ptr, forest::util::index idx) {
  forest::util::check_ptr_valid(ptr);
  return forest::child_node(*ptr, idx);
}
// [[Rcpp::export]]
forest::forest_node forest_subtree__child_node(Rcpp::XPtr<forest::forest_subtree> ptr, forest::util::index idx) {
  forest::util::check_ptr_valid(ptr);
  return forest::child_node(*ptr, idx);
}
// [[Rcpp::export]]
void forest_tree__set_child_node(Rcpp::XPtr<forest::forest_tree> ptr, forest::util::index idx, forest::forest_node value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_child_node(*ptr, idx, value);
}
// [[Rcpp::export]]
void forest_subtree__set_child_node(Rcpp::XPtr<forest::forest_subtree> ptr, forest::util::index idx, forest::forest_node value) {
  forest::util::check_ptr_valid(ptr);
  forest::set_child_node(*ptr, idx, value);
}
// [[Rcpp::export]]
forest::forest_subtree forest_tree__subtree(Rcpp::XPtr<forest::forest_tree> ptr, std::string label) {
  forest::util::check_ptr_valid(ptr);
  return forest::subtree(*ptr, label);
}
// [[Rcpp::export]]
forest::forest_subtree forest_subtree__subtree(Rcpp::XPtr<forest::forest_subtree> ptr, std::string label) {
  forest::util::check_ptr_valid(ptr);
  return forest::subtree(*ptr, label);
}
//[[[end]]]
