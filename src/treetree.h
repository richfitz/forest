#ifndef _FOREST_TREETREE_H_
#define _FOREST_TREETREE_H_

#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're treetree issues.
#pragma clang diagnostic ignored "-Wshadow"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wsign-conversion"
#endif
#define TREE_TREE_NAMESPACE treetree
#include "treetree/tree.hpp"
#include "treetree/tree_io.hpp"
#ifdef __clang__
#pragma clang diagnostic pop
#endif

#endif
