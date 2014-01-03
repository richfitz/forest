#include <Rcpp.h>
#include "tree.h"

// Automatically depend on BH, set up linking.

// [[Rcpp::depends(BH)]]


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
    .property("childless", &itree::childless)
    .property("representation", &itree::representation)

    .method("insert_end", &itree::insert_end)
    .method("insert_end_child", &itree::insert_end_child)
    ;
}

// RCPP_EXPORTED_CLASS(forest::itree)
