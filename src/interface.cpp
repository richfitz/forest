#include <Rcpp.h>
#include "tree.h"

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

    .method("insert_end", &itree::insert_end)
    .method("insert_end_child", &itree::insert_end_child)

    .method("insert_at_node", &itree::insert_at_node)
    ;
}

// RCPP_EXPORTED_CLASS(forest::itree)
