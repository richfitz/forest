#include <Rcpp.h>
#include "tree.h"
#include "iterator_wrapper.h"

// Iterators

// [const_]pre_iterator       -- over *data*, in preorder
// [const_]sub_pre_iterator   -- over *subtrees* in preorder
// [const_]child_iterator     -- over *data*, over daughters of a node
// [const_]sub_child_iterator -- over *subtrees*, over daughters of a node
// [const_]post_iterator      -- over *data*, in preorder
// [const_]sub_post_iterator  -- over *subtrees*, in preorder

// Following test_runner.cpp, we'll define some basic fully-specified
// tree types here:
typedef forest::tree< int > itree;
typedef itree::node_type    inode;
RCPP_EXPOSED_CLASS_NODECL(itree)
RCPP_EXPOSED_CLASS_NODECL(inode)
FOREST_ITERATOR_EXPORT(itree::pre_iterator)
FOREST_ITERATOR_EXPORT(itree::post_iterator)
FOREST_ITERATOR_EXPORT(itree::child_iterator)

// typedef forest::subtree<int> isubtree;

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

    .property("index",     &itree::index)
    .property("indices",   &itree::indices)

    .method("insert_at_node",     &itree::insert_at_node)
    .method("insert_root",        &itree::insert_root)
    .method("insert_at_iterator", &itree::insert_at_iterator)

    .method("clone",       &itree::clone)

    .method("is_equal_to",     &itree::is_equal_to)

    .method("begin",           &itree::begin)
    .method("end",             &itree::end)
    .method("begin_post",      &itree::begin_post)
    .method("end_post",        &itree::end_post)
    .method("begin_child",     &itree::begin_child)
    .method("end_child",       &itree::end_child)
    ;

  FOREST_ITERATOR_MODULE(itree::pre_iterator, "itree_pre_iterator")
  FOREST_ITERATOR_MODULE(itree::post_iterator, "itree_post_iterator")
  FOREST_ITERATOR_MODULE(itree::child_iterator, "itree_child_iterator")
}
