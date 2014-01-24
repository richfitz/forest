#include <Rcpp.h>
#include "tree.hpp"
#include "iterator_wrapper.hpp"
#include "iterator_wrapper_algorithm.hpp"

// For testing purposes, work with a tree where every node contains just
// an integer.
typedef int node;

typedef forest::tree_wrapped<node>          itree_wrapped;
typedef itree_wrapped::subtree_wrapped_type isubtree_wrapped;
typedef itree_wrapped::tree_type            itree;
typedef itree_wrapped::subtree_type         isubtree;

RCPP_EXPOSED_CLASS_NODECL(itree_wrapped)
RCPP_EXPOSED_CLASS_NODECL(isubtree_wrapped)

FOREST_ITERATOR_EXPORT(itree::pre_iterator)
FOREST_ITERATOR_EXPORT(itree::post_iterator)
FOREST_ITERATOR_EXPORT(itree::child_iterator)
FOREST_ITERATOR_EXPORT(itree::sub_pre_iterator)
FOREST_ITERATOR_EXPORT(itree::sub_post_iterator)
FOREST_ITERATOR_EXPORT(itree::sub_child_iterator)

namespace Rcpp {
template<> SEXP wrap(const itree& obj);
template<> SEXP wrap(const itree& obj) {
  return Rcpp::wrap(itree_wrapped(obj));
}
template<> itree as(SEXP obj);
template<> itree as(SEXP obj) {
  itree_wrapped st = Rcpp::as<itree_wrapped>(obj);
  return st.tree_;
}

template<> SEXP wrap(const isubtree& obj);
template<> SEXP wrap(const isubtree& obj) {
  return Rcpp::wrap(isubtree_wrapped(obj));
}
template<> isubtree as(SEXP obj);
template<> isubtree as(SEXP obj) {
  isubtree_wrapped st = Rcpp::as<isubtree_wrapped>(obj);
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
RCPP_MODULE(tree_test) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<itree_wrapped>("itree")
    .constructor()
    .constructor<itree_wrapped::value_type>()
    .method("copy",              &itree_wrapped::copy)
    .method("clear",             &itree_wrapped::clear)

    // 1. Basic interrogation
    .property("empty",           &itree_wrapped::empty)
    .property("size",            &itree_wrapped::size)
    .property("arity",           &itree_wrapped::arity)
    .property("childless",       &itree_wrapped::childless)
    .property("representation",  &itree_wrapped::representation)

    // 2. Accessors
    .method("root",              &itree_wrapped::root)
    .method("front",             &itree_wrapped::front)
    .method("back",              &itree_wrapped::back)
    .method("root_sub",          &itree_wrapped::root_sub)
    .method("front_sub",         &itree_wrapped::front_sub)
    .method("back_sub",          &itree_wrapped::back_sub)

    // NOTE: These are base-1
    .method("at",                &itree_wrapped::r_at)
    .method("[[",                &itree_wrapped::r_at)
    .method("insert_at",         &itree_wrapped::r_insert_at)
    .method("[[<-",              &itree_wrapped::r_insert_at)

    // 3. Iterators
    .method("begin",             &itree_wrapped::begin)
    .method("end",               &itree_wrapped::end)
    .method("begin_post",        &itree_wrapped::begin_post)
    .method("end_post",          &itree_wrapped::end_post)
    .method("begin_child",       &itree_wrapped::begin_child)
    .method("end_child",         &itree_wrapped::end_child)

    .method("begin_sub",         &itree_wrapped::begin_sub)
    .method("end_sub",           &itree_wrapped::end_sub)
    .method("begin_sub_post",    &itree_wrapped::begin_sub_post)
    .method("end_sub_post",      &itree_wrapped::end_sub_post)
    .method("begin_sub_child",   &itree_wrapped::begin_sub_child)
    .method("end_sub_child",     &itree_wrapped::end_sub_child)

    // 4. Insert
    .method("insert",            &itree_wrapped::insert)
    .method("insert_subtree",    &itree_wrapped::insert_subtree)
    .method("insert_n",          &itree_wrapped::insert_n)
    .method("insert_subtree_n",  &itree_wrapped::insert_subtree_n)

    .method("insert_above",      &itree_wrapped::insert_above)
    .method("insert_below",      &itree_wrapped::insert_below)

    // 5. Append + Prepend
    .method("append",            &itree_wrapped::append)
    .method("prepend",           &itree_wrapped::prepend)
    .method("append_subtree",    &itree_wrapped::append_subtree)
    .method("prepend_subtree",   &itree_wrapped::prepend_subtree)

    .method("append_n",          &itree_wrapped::append_n)
    .method("prepend_n",         &itree_wrapped::prepend_n)
    .method("append_subtree_n",  &itree_wrapped::append_subtree_n)
    .method("prepend_subtree_n", &itree_wrapped::prepend_subtree_n)

    // 6. Splice
    .method("splice",            &itree_wrapped::splice)
    .method("splice_pair",       &itree_wrapped::splice_pair)

    // 7. Destructive modification
    .method("prune",             &itree_wrapped::prune)
    .method("flatten",           &itree_wrapped::flatten)
    .method("erase",             &itree_wrapped::erase)
    .method("erase_pair",        &itree_wrapped::erase_pair)

    // 8. Equality testing
    .method("equals",            &itree_wrapped::operator==)

    // Extra
    .property("tips",            &itree_wrapped::tips)
    .property("nodes",           &itree_wrapped::nodes)
    .property("tip_labels",      &itree_wrapped::tip_labels)
    .property("node_labels",     &itree_wrapped::node_labels)
    ;

  Rcpp::class_<isubtree_wrapped>("isubtree")
    // NOTE: no constructor, copy, clear

    // 1. Basic interrogation
    .property("empty",           &isubtree_wrapped::empty)
    .property("size",            &isubtree_wrapped::size)
    .property("arity",           &isubtree_wrapped::arity)
    .property("childless",       &isubtree_wrapped::childless)
    .property("representation",  &isubtree_wrapped::representation)

    // 2. Accessors
    .method("root",              &isubtree_wrapped::root)
    .method("front",             &isubtree_wrapped::front)
    .method("back",              &isubtree_wrapped::back)
    .method("root_sub",          &isubtree_wrapped::root_sub)
    .method("front_sub",         &isubtree_wrapped::front_sub)
    .method("back_sub",          &isubtree_wrapped::back_sub)

    // NOTE: These are base-1
    .method("at",                &isubtree_wrapped::r_at)
    .method("[[",                &isubtree_wrapped::r_at)

    // 3. Iterators
    .method("begin",             &isubtree_wrapped::begin)
    .method("end",               &isubtree_wrapped::end)
    .method("begin_post",        &isubtree_wrapped::begin_post)
    .method("end_post",          &isubtree_wrapped::end_post)
    .method("begin_child",       &isubtree_wrapped::begin_child)
    .method("end_child",         &isubtree_wrapped::end_child)

    .method("begin_sub",         &isubtree_wrapped::begin_sub)
    .method("end_sub",           &isubtree_wrapped::end_sub)
    .method("begin_sub_post",    &isubtree_wrapped::begin_sub_post)
    .method("end_sub_post",      &isubtree_wrapped::end_sub_post)
    .method("begin_sub_child",   &isubtree_wrapped::begin_sub_child)
    .method("end_sub_child",     &isubtree_wrapped::end_sub_child)

    // 4. Insert
    .method("insert",            &isubtree_wrapped::insert)
    .method("insert_subtree",    &isubtree_wrapped::insert_subtree)
    .method("insert_n",          &isubtree_wrapped::insert_n)
    .method("insert_subtree_n",  &isubtree_wrapped::insert_subtree_n)

    .method("insert_above",      &isubtree_wrapped::insert_above)
    .method("insert_below",      &isubtree_wrapped::insert_below)

    // 5. Append + Prepend
    .method("append",            &isubtree_wrapped::append)
    .method("prepend",           &isubtree_wrapped::prepend)
    .method("append_subtree",    &isubtree_wrapped::append_subtree)
    .method("prepend_subtree",   &isubtree_wrapped::prepend_subtree)

    .method("append_n",          &isubtree_wrapped::append_n)
    .method("prepend_n",         &isubtree_wrapped::prepend_n)
    .method("append_subtree_n",  &isubtree_wrapped::append_subtree_n)
    .method("prepend_subtree_n", &isubtree_wrapped::prepend_subtree_n)

    // 6. Splice
    .method("splice",            &isubtree_wrapped::splice)
    .method("splice_pair",       &isubtree_wrapped::splice_pair)

    // 7. Destructive modification
    .method("prune",             &isubtree_wrapped::prune)
    .method("flatten",           &isubtree_wrapped::flatten)
    .method("erase",             &isubtree_wrapped::erase)
    .method("erase_pair",        &isubtree_wrapped::erase_pair)

    // 8. Equality testing
    .method("equals",            &isubtree_wrapped::operator==)

    // Extra
    .property("tips",            &isubtree_wrapped::tips)
    .property("nodes",           &isubtree_wrapped::nodes)
    .property("tip_labels",      &isubtree_wrapped::tip_labels)
    .property("node_labels",     &isubtree_wrapped::node_labels)
    ;

  // NOTE: We don't have to export the subtree iterators separately
  // because they are actually the same type as the tree iterators.
  FOREST_ITERATOR_MODULE(itree::pre_iterator,       "itree_pre_iterator")
  FOREST_ITERATOR_MODULE(itree::post_iterator,      "itree_post_iterator")
  FOREST_ITERATOR_MODULE(itree::child_iterator,     "itree_child_iterator")
  FOREST_ITERATOR_MODULE(itree::sub_pre_iterator,   "itree_sub_pre_iterator")
  FOREST_ITERATOR_MODULE(itree::sub_post_iterator,  "itree_sub_post_iterator")
  FOREST_ITERATOR_MODULE(itree::sub_child_iterator, "itree_sub_child_iterator")

  FOREST_ITERATOR_MODULE_ALGORITHM(itree::pre_iterator,
				   "itree_pre_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(itree::post_iterator,
				   "itree_post_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(itree::child_iterator,
				   "itree_child_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(itree::sub_pre_iterator,
				   "itree_sub_pre_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(itree::sub_post_iterator,
				   "itree_sub_post_iterator")
  FOREST_ITERATOR_MODULE_ALGORITHM(itree::sub_child_iterator,
				   "itree_sub_child_iterator")

  // Useful utility function, but only exported for pre_iterators
  Rcpp::function("parent_itree_iterator",
		 &treetree::parent<itree::pre_iterator>);
}
