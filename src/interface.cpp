#include <Rcpp.h>
#include "tree.h"
#include "iterator_wrapper.h"
#include "iterator_wrapper_algorithm.h"

// Iterators

// Pointing at nodes (node iterators)
//   [const_]pre_iterator       -- in preorder
//   [const_]post_iterator      -- in preorder
//   [const_]child_iterator     -- over daughters of a node
//
// Pointing  at subtrees (subtree iterators)
//   [const_]sub_pre_iterator   -- in preorder
//   [const_]sub_post_iterator  -- in preorder
//   [const_]sub_child_iterator -- over daughters of a node

// Following test_runner.cpp, we'll define some basic fully-specified
// tree types here:
typedef forest::tree< int > itree;
typedef itree::node_type    inode;
RCPP_EXPOSED_CLASS_NODECL(itree)
RCPP_EXPOSED_CLASS_NODECL(inode)
FOREST_ITERATOR_EXPORT(itree::pre_iterator)
FOREST_ITERATOR_EXPORT(itree::post_iterator)
FOREST_ITERATOR_EXPORT(itree::child_iterator)

typedef forest::subtree_wrapped<int> isubtree_wrapped;
typedef itree::subtree_type isubtree;
RCPP_EXPOSED_CLASS_NODECL(isubtree_wrapped)

FOREST_ITERATOR_EXPORT(itree::sub_pre_iterator)
FOREST_ITERATOR_EXPORT(itree::sub_post_iterator)
FOREST_ITERATOR_EXPORT(itree::sub_child_iterator)

// In contrast to the iterator versions, these may be possible to
// template for all subtree types.  However, I did try doing this with
// little success (see nodes.md)
namespace Rcpp {
template<> SEXP wrap(const isubtree& obj);
template<> SEXP wrap(const isubtree& obj) {
  return Rcpp::wrap(isubtree_wrapped::create(obj));
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
RCPP_MODULE(forest) {
#ifdef __clang__
#pragma clang diagnostic pop
#endif
  Rcpp::class_<inode>("inode")
    .property("index", &inode::index)
    .field("data",     &inode::data)
    ;

  Rcpp::class_<itree>("itree")
    .constructor()
    .constructor<int>()

    .property("empty",     &itree::empty)
    .property("size",      &itree::size)
    .property("arity",     &itree::arity)
    .property("childless", &itree::childless)
    .property("representation", &itree::representation)

    .method("root",        &itree::root)
    .method("front",       &itree::front)
    .method("back",        &itree::back)
    .method("root_sub",    &itree::root_sub)
    .method("front_sub",   &itree::front_sub)
    .method("back_sub",    &itree::back_sub)

    .method("insert",           &itree::insert)
    .method("insert_subtree",   &itree::insert_subtree)
    .method("insert_n",         &itree::insert_n)
    .method("insert_subtree_n", &itree::insert_subtree_n)

    .method("append_node",     &itree::append_node)
    .method("prepend_node",    &itree::prepend_node)
    .method("append_subtree",  &itree::append_subtree)
    .method("prepend_subtree", &itree::prepend_subtree)

    .method("insert_above",    &itree::insert_above)
    .method("insert_below",    &itree::insert_below)

    .method("flatten",         &itree::flatten)

    .method("copy",       &itree::copy)

    .method("erase",           &itree::erase)
    .method("erase_pair",      &itree::erase_pair)
    .method("prune",           &itree::prune)
    .method("clear",           &itree::clear)

    .method("splice",          &itree::splice)
    .method("splice_pair",     &itree::splice_pair)

    .method("is_equal_to",     &itree::is_equal_to)

    .method("begin",           &itree::begin)
    .method("end",             &itree::end)
    .method("begin_post",      &itree::begin_post)
    .method("end_post",        &itree::end_post)
    .method("begin_child",     &itree::begin_child)
    .method("end_child",       &itree::end_child)

    .method("begin_sub",       &itree::begin_sub)
    .method("end_sub",         &itree::end_sub)
    .method("begin_sub_post",  &itree::begin_sub_post)
    .method("end_sub_post",    &itree::end_sub_post)
    .method("begin_sub_child", &itree::begin_sub_child)
    .method("end_sub_child",   &itree::end_sub_child)

    // NOTE: using base-1 accessor/setter here.
    .method("at",              &itree::r_at)
    .method("[[",              &itree::r_at)
    .method("insert_at",       &itree::r_insert_at)
    .method("[[<-",            &itree::r_insert_at)
    ;

  Rcpp::class_<isubtree_wrapped>("isubtree_wrapped")
    .property("empty",          &isubtree_wrapped::empty)
    .property("size",           &isubtree_wrapped::size)
    .property("arity",          &isubtree_wrapped::arity)
    .property("childless",      &isubtree_wrapped::childless)
    .property("representation", &isubtree_wrapped::representation)

    .method("insert",           &isubtree_wrapped::insert)
    .method("insert_subtree",   &isubtree_wrapped::insert_subtree)
    .method("insert_n",         &isubtree_wrapped::insert_n)
    .method("insert_subtree_n", &isubtree_wrapped::insert_subtree_n)

    .method("append_node",      &isubtree_wrapped::append_node)
    .method("prepend_node",     &isubtree_wrapped::prepend_node)
    .method("append_subtree",   &isubtree_wrapped::append_subtree)
    .method("prepend_subtree",  &isubtree_wrapped::prepend_subtree)

    .method("insert_above",     &isubtree_wrapped::insert_above)
    .method("insert_below",     &isubtree_wrapped::insert_below)

    .method("flatten",          &isubtree_wrapped::flatten)

    .method("erase",            &isubtree_wrapped::erase)
    .method("erase_pair",       &isubtree_wrapped::erase_pair)
    .method("prune",            &isubtree_wrapped::prune)

    .method("splice",           &isubtree_wrapped::splice)
    .method("splice_pair",      &isubtree_wrapped::splice_pair)

    .method("is_equal_to",      &isubtree_wrapped::is_equal_to)

    .method("begin",            &isubtree_wrapped::begin)
    .method("end",              &isubtree_wrapped::end)
    .method("begin_post",       &isubtree_wrapped::begin_post)
    .method("end_post",         &isubtree_wrapped::end_post)
    .method("begin_child",      &isubtree_wrapped::begin_child)
    .method("end_child",        &isubtree_wrapped::end_child)

    // NOTE: using base-1 accessor/setter here.
    .method("at",               &isubtree_wrapped::r_at)
    .method("[[",               &isubtree_wrapped::r_at)
    ;

  FOREST_ITERATOR_MODULE(itree::pre_iterator, "itree_pre_iterator")
  FOREST_ITERATOR_MODULE(itree::post_iterator, "itree_post_iterator")
  FOREST_ITERATOR_MODULE(itree::child_iterator, "itree_child_iterator")
  FOREST_ITERATOR_MODULE(itree::sub_pre_iterator, "itree_sub_pre_iterator")
  FOREST_ITERATOR_MODULE(itree::sub_post_iterator, "itree_sub_post_iterator")
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

  Rcpp::function("parent",
		 &TREE_TREE_NAMESPACE::parent<itree::pre_iterator>);
}
