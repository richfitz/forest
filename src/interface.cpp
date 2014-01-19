#include <Rcpp.h>
#include "tree.hpp"
#include "iterator_wrapper.hpp"
#include "iterator_wrapper_algorithm.hpp"

#include "node.hpp"

#include "newick.hpp" // tree io
#include "ape.hpp"    // ape conversion

typedef forest::node<Rcpp::RObject> xnode;
RCPP_EXPOSED_CLASS_NODECL(xnode)

// All this is boilerplate, copied over from interface.cpp, but with
// the substitutions:
//   itree    -> xtree
//   isubtree -> xsubtree
// And with the definition/export of xnode (above) and module export
// for xnode below.

typedef forest::tree_wrapped<xnode>         xtree_wrapped;
typedef xtree_wrapped::subtree_wrapped_type xsubtree_wrapped;
typedef xtree_wrapped::tree_type            xtree;
typedef xtree_wrapped::subtree_type         xsubtree;

RCPP_EXPOSED_CLASS_NODECL(xtree_wrapped)
RCPP_EXPOSED_CLASS_NODECL(xsubtree_wrapped)

FOREST_ITERATOR_EXPORT(xtree::pre_iterator)
FOREST_ITERATOR_EXPORT(xtree::post_iterator)
FOREST_ITERATOR_EXPORT(xtree::child_iterator)
FOREST_ITERATOR_EXPORT(xtree::sub_pre_iterator)
FOREST_ITERATOR_EXPORT(xtree::sub_post_iterator)
FOREST_ITERATOR_EXPORT(xtree::sub_child_iterator)

namespace Rcpp {
template<> SEXP wrap(const xtree& obj);
template<> SEXP wrap(const xtree& obj) {
  return Rcpp::wrap(xtree_wrapped(obj));
}
template<> xtree as(SEXP obj);
template<> xtree as(SEXP obj) {
  xtree_wrapped st = Rcpp::as<xtree_wrapped>(obj);
  return st.tree_;
}

template<> SEXP wrap(const xsubtree& obj);
template<> SEXP wrap(const xsubtree& obj) {
  return Rcpp::wrap(xsubtree_wrapped(obj));
}
template<> xsubtree as(SEXP obj);
template<> xsubtree as(SEXP obj) {
  xsubtree_wrapped st = Rcpp::as<xsubtree_wrapped>(obj);
  return st.subtree_;
}
}

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
  Rcpp::class_<xnode>("xnode")
    .constructor()
    .constructor<std::string>()
    .constructor<std::string, double>()
    .constructor<std::string, double, xnode::value_type>()
    .property("has_label",    &xnode::has_label)
    .property("has_length",   &xnode::has_length)
    .field("data",            &xnode::data_)
    .field("label",           &xnode::label_)
    .field("length",          &xnode::length_)
    .field_readonly("height", &xnode::height_)
    .field_readonly("depth",  &xnode::depth_)
    .method("copy",           &xnode::copy)
    .method("equals",         &xnode::operator==)
    ;

  Rcpp::class_<xtree_wrapped>("xtree")
    .constructor()
    .constructor<xtree_wrapped::value_type>()
    .method("copy",              &xtree_wrapped::copy)
    .method("clear",             &xtree_wrapped::clear)

    // 1. Basic interrogation
    .property("empty",           &xtree_wrapped::empty)
    .property("size",            &xtree_wrapped::size)
    .property("arity",           &xtree_wrapped::arity)
    .property("childless",       &xtree_wrapped::childless)
    .property("representation",  &xtree_wrapped::representation)

    // 2. Accessors
    .method("root",              &xtree_wrapped::root)
    .method("front",             &xtree_wrapped::front)
    .method("back",              &xtree_wrapped::back)
    .method("root_sub",          &xtree_wrapped::root_sub)
    .method("front_sub",         &xtree_wrapped::front_sub)
    .method("back_sub",          &xtree_wrapped::back_sub)

    // NOTE: These are base-1
    .method("at",                &xtree_wrapped::r_at)
    .method("[[",                &xtree_wrapped::r_at)
    .method("insert_at",         &xtree_wrapped::r_insert_at)
    .method("[[<-",              &xtree_wrapped::r_insert_at)

    // 3. Iterators
    .method("begin",             &xtree_wrapped::begin)
    .method("end",               &xtree_wrapped::end)
    .method("begin_post",        &xtree_wrapped::begin_post)
    .method("end_post",          &xtree_wrapped::end_post)
    .method("begin_child",       &xtree_wrapped::begin_child)
    .method("end_child",         &xtree_wrapped::end_child)

    .method("begin_sub",         &xtree_wrapped::begin_sub)
    .method("end_sub",           &xtree_wrapped::end_sub)
    .method("begin_sub_post",    &xtree_wrapped::begin_sub_post)
    .method("end_sub_post",      &xtree_wrapped::end_sub_post)
    .method("begin_sub_child",   &xtree_wrapped::begin_sub_child)
    .method("end_sub_child",     &xtree_wrapped::end_sub_child)

    // 4. Insert
    .method("insert",            &xtree_wrapped::insert)
    .method("insert_subtree",    &xtree_wrapped::insert_subtree)
    .method("insert_n",          &xtree_wrapped::insert_n)
    .method("insert_subtree_n",  &xtree_wrapped::insert_subtree_n)

    .method("insert_above",      &xtree_wrapped::insert_above)
    .method("insert_below",      &xtree_wrapped::insert_below)

    // 5. Append + Prepend
    .method("append",            &xtree_wrapped::append)
    .method("prepend",           &xtree_wrapped::prepend)
    .method("append_subtree",    &xtree_wrapped::append_subtree)
    .method("prepend_subtree",   &xtree_wrapped::prepend_subtree)

    .method("append_n",          &xtree_wrapped::append_n)
    .method("prepend_n",         &xtree_wrapped::prepend_n)
    .method("append_subtree_n",  &xtree_wrapped::append_subtree_n)
    .method("prepend_subtree_n", &xtree_wrapped::prepend_subtree_n)

    // 6. Splice
    .method("splice",            &xtree_wrapped::splice)
    .method("splice_pair",       &xtree_wrapped::splice_pair)

    // 7. Destructive modification
    .method("prune",             &xtree_wrapped::prune)
    .method("flatten",           &xtree_wrapped::flatten)
    .method("erase",             &xtree_wrapped::erase)
    .method("erase_pair",        &xtree_wrapped::erase_pair)

    // 8. Equality testing
    .method("equals",            &xtree_wrapped::operator==)

    .method("is_node_type",      &xtree_wrapped::is_node_type)

    // Extra
    .property("tips",            &xtree_wrapped::tips)
    .property("nodes",           &xtree_wrapped::nodes)
    .property("tip_labels",      &xtree_wrapped::tip_labels)
    .property("node_labels",     &xtree_wrapped::node_labels)
    .property("heights",         &xtree_wrapped::heights)
    .property("depths",          &xtree_wrapped::depths)
    .property("is_binary",
              &xtree_wrapped::is_binary)

    // Extra -- full tree only and require a tree< node<T> >
    .property("has_branch_lengths",
              &xtree_wrapped::has_branch_lengths)
    .method("is_ultrametric",
            &xtree_wrapped::is_ultrametric)
    .method("update_heights",    &xtree_wrapped::update_heights)

    .method("collapse_singles",  &xtree_wrapped::collapse_singles)
    .method("drop_tips",         &xtree_wrapped::drop_tips)
    ;

  Rcpp::class_<xsubtree_wrapped>("xsubtree")
    // NOTE: no constructor, copy, clear

    // 1. Basic interrogation
    .property("empty",           &xsubtree_wrapped::empty)
    .property("size",            &xsubtree_wrapped::size)
    .property("arity",           &xsubtree_wrapped::arity)
    .property("childless",       &xsubtree_wrapped::childless)
    .property("representation",  &xsubtree_wrapped::representation)

    // 2. Accessors
    .method("root",              &xsubtree_wrapped::root)
    .method("front",             &xsubtree_wrapped::front)
    .method("back",              &xsubtree_wrapped::back)
    .method("root_sub",          &xsubtree_wrapped::root_sub)
    .method("front_sub",         &xsubtree_wrapped::front_sub)
    .method("back_sub",          &xsubtree_wrapped::back_sub)

    // NOTE: These are base-1
    .method("at",                &xsubtree_wrapped::r_at)
    .method("[[",                &xsubtree_wrapped::r_at)

    // 3. Iterators
    .method("begin",             &xsubtree_wrapped::begin)
    .method("end",               &xsubtree_wrapped::end)
    .method("begin_post",        &xsubtree_wrapped::begin_post)
    .method("end_post",          &xsubtree_wrapped::end_post)
    .method("begin_child",       &xsubtree_wrapped::begin_child)
    .method("end_child",         &xsubtree_wrapped::end_child)

    .method("begin_sub",         &xsubtree_wrapped::begin_sub)
    .method("end_sub",           &xsubtree_wrapped::end_sub)
    .method("begin_sub_post",    &xsubtree_wrapped::begin_sub_post)
    .method("end_sub_post",      &xsubtree_wrapped::end_sub_post)
    .method("begin_sub_child",   &xsubtree_wrapped::begin_sub_child)
    .method("end_sub_child",     &xsubtree_wrapped::end_sub_child)

    // 4. Insert
    .method("insert",            &xsubtree_wrapped::insert)
    .method("insert_subtree",    &xsubtree_wrapped::insert_subtree)
    .method("insert_n",          &xsubtree_wrapped::insert_n)
    .method("insert_subtree_n",  &xsubtree_wrapped::insert_subtree_n)

    .method("insert_above",      &xsubtree_wrapped::insert_above)
    .method("insert_below",      &xsubtree_wrapped::insert_below)

    // 5. Append + Prepend
    .method("append",            &xsubtree_wrapped::append)
    .method("prepend",           &xsubtree_wrapped::prepend)
    .method("append_subtree",    &xsubtree_wrapped::append_subtree)
    .method("prepend_subtree",   &xsubtree_wrapped::prepend_subtree)

    .method("append_n",          &xsubtree_wrapped::append_n)
    .method("prepend_n",         &xsubtree_wrapped::prepend_n)
    .method("append_subtree_n",  &xsubtree_wrapped::append_subtree_n)
    .method("prepend_subtree_n", &xsubtree_wrapped::prepend_subtree_n)

    // 6. Splice
    .method("splice",            &xsubtree_wrapped::splice)
    .method("splice_pair",       &xsubtree_wrapped::splice_pair)

    // 7. Destructive modification
    .method("prune",             &xsubtree_wrapped::prune)
    .method("flatten",           &xsubtree_wrapped::flatten)
    .method("erase",             &xsubtree_wrapped::erase)
    .method("erase_pair",        &xsubtree_wrapped::erase_pair)

    // 8. Equality testing
    .method("equals",            &xsubtree_wrapped::operator==)

    // Extra
    .property("tips",            &xsubtree_wrapped::tips)
    .property("nodes",           &xsubtree_wrapped::nodes)
    .property("tip_labels",      &xsubtree_wrapped::tip_labels)
    .property("node_labels",     &xsubtree_wrapped::node_labels)
    .property("heights",         &xsubtree_wrapped::heights)
    .property("depths",          &xsubtree_wrapped::depths)
    ;

  // NOTE: We don't have to export the subtree iterators separately
  // because they are actually the same type as the tree iterators.
  FOREST_ITERATOR_MODULE(xtree::pre_iterator,       "xtree_pre_iterator")
  FOREST_ITERATOR_MODULE(xtree::post_iterator,      "xtree_post_iterator")
  FOREST_ITERATOR_MODULE(xtree::child_iterator,     "xtree_child_iterator")
  FOREST_ITERATOR_MODULE(xtree::sub_pre_iterator,   "xtree_sub_pre_iterator")
  FOREST_ITERATOR_MODULE(xtree::sub_post_iterator,  "xtree_sub_post_iterator")
  FOREST_ITERATOR_MODULE(xtree::sub_child_iterator, "xtree_sub_child_iterator")

  FOREST_ITERATOR_MODULE_ALGORITHM(xtree::pre_iterator,
				   "xtree_pre_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(xtree::post_iterator,
				   "xtree_post_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(xtree::child_iterator,
				   "xtree_child_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(xtree::sub_pre_iterator,
				   "xtree_sub_pre_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(xtree::sub_post_iterator,
				   "xtree_sub_post_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(xtree::sub_child_iterator,
				   "xtree_sub_child_iterator")

  // Useful utility function, but only exported for pre_iterators
  Rcpp::function("parent_xtree_iterator",
		 &treetree::parent<xtree::pre_iterator>);
  Rcpp::function("to_newick_string",
                 &forest::to_newick_string<xtree::value_type>);
  Rcpp::function("from_newick_node",
                 &forest::from_newick_node<xtree::value_type>);
  Rcpp::function("from_newick_string",
                 &forest::from_newick_string<xtree::value_type>);
  Rcpp::function("from_ape_internal",
                 &forest::from_ape_internal<xtree::value_type>);
  Rcpp::function("to_ape_internal",
                 &forest::to_ape_internal<xtree::value_type>);
}
