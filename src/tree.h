// -*-c++-*-
#ifndef _FOREST_TREE_H_
#define _FOREST_TREE_H_

// #define TREE_TREE_NAMESPACE treetree


#ifdef __clang__
#pragma clang diagnostic push
// These I have no control over because they're treetree issues.
#pragma clang diagnostic ignored "-Wshadow"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wsign-conversion"
#endif
#include "treetree/tree.hpp"
#include "treetree/tree_io.hpp"
#ifdef __clang__
#pragma clang diagnostic pop
#endif

namespace forest {

// I've chosen to wrap around the::tree, rather than extend, because
// this way should be easier to expose just a controlled subset, add
// R-specific checks, etc.

// This is going to be particularly important when we need to do
// something like "insert at a particular node", because generating
// safe subtrees is going to be hard.

// A way of thinking about this is to think about what the "type" that
// .end() and end_child() should return.  At the moment, they are set
// up as iterators, but that is going to take some serious care not to
// invalidate.

template <typename T>
struct node {
public:
  node(const T& data_, size_t index_) : index(index_), data(data_) {}
  // This is just to make the actual creation a little easier.
  static node create(const T& data_, size_t index_) {
    node ret(data_, index_);
    return ret;
  }
  const size_t index;
  T      data;
};

template<typename T>
std::ostream& operator<<(std::ostream& out, const forest::node<T>& nd) {
  out << nd.data;
  return out;
}

template <typename T>
class tree {
public:
  tree() : index(0) {}
  tree(const T& t) : tree_(node<T>::create(t, 0)), index(1) {}

  // Basic interrogation methods; pass through to the tree:
  bool empty() const {return tree_.empty();}
  size_t size() const {return tree_.size();}
  bool childless() const {return tree_.childless();}
  std::string representation() const {
    return boost::lexical_cast<std::string>(tree_);}

  // These are horribly inflexible, but will come in useful for
  // testing.  We'll need some other way of indexing nodes that works
  // with R soon enough.
  void insert_end(const T& t) { insert(tree_.end(), t);}
  void insert_end_child(const T& t) { insert(tree_.end_child(), t);}

  // This takes care of the actual inserts, updating the index as
  // needed.
  template<typename Iterator>
  void insert(Iterator i, const T& v) {
    tree_.insert(i, node<T>::create(v, index++));
  }

private:
  TREE_TREE_NAMESPACE::tree< node<T> > tree_;
  size_t index;
};

}

#endif
