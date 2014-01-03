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
  T data;
};

template<typename T>
std::ostream& operator<<(std::ostream& out, const forest::node<T>& nd) {
  out << nd.data;
  return out;
}

template <typename T>
class tree {
public:
  tree() : index_(0) {}
  tree(const T& t) : tree_(node<T>::create(t, 0)), index_(1) {}

  // Basic interrogation methods; pass through to the tree:
  bool empty() const {return tree_.empty();}
  size_t size() const {return tree_.size();}
  size_t arity() const {return tree_.arity();}
  bool childless() const {return tree_.childless();}
  std::string representation() const {
    return boost::lexical_cast<std::string>(tree_);
  }

  // I don't think that this is the best way to do this, but it will
  // work for now, until we work out how subtrees will go.
  size_t index() const {
    if (empty())
      ::Rf_error("Can't get index of empty tree");
    return tree_.root().index;
  }

  // This is all indices within the tree:
  std::vector<size_t> indices() const {
    std::vector<size_t> ret;
    const_pre_iterator it = tree_.begin();
    while (it != tree_.end())
      ret.push_back((it++)->index);
    return ret;
  }

  // We'd also benefit for same to get the child indices.  But that
  // would be much more useful if we also had a decent way of jumping
  // straight to nodes (which I'll probably do in the way that lookup
  // works, using the set of indices and pointers to the different
  // nodes, in a map).
  void insert_at_node(size_t i, const T& t) {
    sub_pre_iterator it = find_node(i, tree_.begin_sub(), tree_.end_sub());
    insert(it->end_child(), t);
  }

  // These are horribly inflexible, but will come in useful for
  // testing.  We'll need some other way of indexing nodes that works
  // with R soon enough.
  void insert_end(const T& t) { insert(tree_.end(), t);}
  void insert_end_child(const T& t) { insert(tree_.end_child(), t);}

private:
  // This takes care of the actual inserts, updating the index as
  // needed.
  template<typename Iterator>
  void insert(Iterator i, const T& v) {
    tree_.insert(i, node<T>::create(v, index_++));
  }
  template<typename Iterator>
  Iterator find_node(size_t i, Iterator first, Iterator last) {
    while (first != last && first->begin()->index != i)
      ++first;
    if (first == tree_.end())
      ::Rf_error("Did not find index %d", i);
    return first;
  }

  typedef TREE_TREE_NAMESPACE::tree< node<T> > tree_type;
  typedef typename tree_type::pre_iterator           pre_iterator;
  typedef typename tree_type::const_pre_iterator     const_pre_iterator;
  typedef typename tree_type::sub_pre_iterator       sub_pre_iterator;
  typedef typename tree_type::const_sub_pre_iterator const_sub_pre_iterator;

  tree_type tree_;
  size_t index_;
};

}

#endif
