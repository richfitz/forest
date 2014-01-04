// -*-c++-*-
#ifndef _FOREST_TREE_H_
#define _FOREST_TREE_H_

#include <Rcpp.h>

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

template <typename T>
struct node {
public:
  node(const T& data_, size_t index_) : data(data_), _index(index_) {}
  // This is just to make the actual creation a little easier.
  static node create(const T& data_, size_t index_) {
    node ret(data_, index_);
    return ret;
  }
  size_t index() const {return _index;}

  // Basic comparisons -- basically passed down to underlying type,
  // skipping the index.
  bool operator==(const node<T>& rhs) const { return data == rhs.data; }
  bool operator<(const node<T>& rhs) const { return data < rhs.data; }
  bool operator>(const node<T>& rhs) const { return data > rhs.data; }

  T data;

private:
  size_t _index;
};

template<typename T>
std::ostream& operator<<(std::ostream& out, const forest::node<T>& nd) {
  out << nd.data;
  return out;
}

template <typename T>
class tree {
public:
  typedef node<T>                                    node_type;
  typedef TREE_TREE_NAMESPACE::tree<node_type>       tree_type;
  typedef typename tree_type::pre_iterator           pre_iterator;
  typedef typename tree_type::post_iterator          post_iterator;
  typedef typename tree_type::child_iterator         child_iterator;

  typedef typename tree_type::const_pre_iterator     const_pre_iterator;
  typedef typename tree_type::sub_pre_iterator       sub_pre_iterator;
  typedef typename tree_type::const_sub_pre_iterator const_sub_pre_iterator;

  tree() : index_(0) {}
  tree(const T& t) : tree_(node_type::create(t, 0)), index_(1) {}

  // Basic interrogation methods; pass through to the tree:
  bool empty() const {return tree_.empty();}
  size_t size() const {return tree_.size();}
  size_t arity() const {return tree_.arity();}
  bool childless() const {return tree_.childless();}
  std::string representation() const;

  // Index of current (root) node
  size_t index() const;
  // All indices within the tree
  std::vector<size_t> indices() const;

  // Insert a node as a child of node with index 'i'.
  void insert_at_node(size_t i, const T& t);
  // Insert a root node
  void insert_root(const T& t);
  // In theory, this is all that is needed, but we need to be able to
  // deal with all the different iterator types, as they will silently
  // convert from one to the other (this is instantiated in the module
  // code as a pre_iterator).
  void insert_at_iterator(pre_iterator i, const T& t) {insert(i, t);}

  tree<T> clone() const {return *this;}

  bool is_equal_to(const tree<T>& rhs) const;

  pre_iterator   begin()       { return tree_.begin();       }
  pre_iterator   end()         { return tree_.end();         }
  post_iterator  begin_post()  { return tree_.begin_post();  }
  post_iterator  end_post()    { return tree_.end_post();    }
  child_iterator begin_child() { return tree_.begin_child(); }
  child_iterator end_child()   { return tree_.end_child();   }

private:
  // This takes care of the actual inserts, updating the index as
  // needed.
  template<typename Iterator>
  void insert(Iterator i, const T& v);
  template<typename Iterator>
  Iterator find_node(size_t i, Iterator first, Iterator last);

  tree_type tree_;
  size_t index_;
};

template <typename T>
std::string tree<T>::representation() const {
  return boost::lexical_cast<std::string>(tree_);
}

template <typename T>
size_t tree<T>::index() const {
  if (empty())
    ::Rf_error("Can't get index of empty tree");
  return tree_.root().index();
}

template <typename T>
std::vector<size_t> tree<T>::indices() const {
  std::vector<size_t> ret;
  const_pre_iterator it = tree_.begin();
  while (it != tree_.end())
    ret.push_back((it++)->index());
  return ret;
}

template <typename T>
void tree<T>::insert_at_node(size_t i, const T& t) {
  sub_pre_iterator it = find_node(i, tree_.begin_sub(), tree_.end_sub());
  insert(it->end_child(), t);
}

template <typename T>
void tree<T>::insert_root(const T& t) {
  insert(tree_.end(), t);
}


template <typename T>
bool tree<T>::is_equal_to(const tree<T>& rhs) const {
  return this->tree_ == rhs.tree_;
}

template<typename T>
template<typename Iterator>
void tree<T>::insert(Iterator i, const T& v) {
  tree_.insert(i, node_type::create(v, index_++));
}

template<typename T>
template<typename Iterator>
Iterator tree<T>::find_node(size_t i, Iterator first, Iterator last) {
  while (first != last && first->begin()->index() != i)
    ++first;
  if (first == tree_.end())
    ::Rf_error("Did not find index %d", i);
  return first;
}

}

#endif
